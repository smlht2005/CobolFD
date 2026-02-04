/*
 * 更新時間：2026-02-04 11:20
 * 作者：Cursor
 * 摘要：單一 FD 邏輯；CalculateFDSizes/CalculateFDDETs 計算所有 FD，已移除 EXTERNAL 相關
 * 
 * 更新歷程：
 * - 2026-02-04 11:20: 移除 IsExternal 屬性與賦值；方法更名為 CalculateFDDETs/CalculateFDSizes；註解改為 FD layout
 * - 2026-02-04 11:15: 移除 IsExternal 篩選，所有 FD 皆計算 Size/DET
 * - 2026-02-03 18:30: 新增 CobolLayoutAnalyzer，ParseCobolStructure、CountDET、CountSize
 */
using System.Text.RegularExpressions;

namespace CobolLayoutLib
{
    /// <summary>
    /// COBOL Layout 解析與計算（公開 API）
    /// 提供 COBOL 結構解析、DET 計算、Size 計算等功能
    /// </summary>
    public static class CobolLayoutAnalyzer
    {
        #region Public API - DET 計算

        /// <summary>
        /// 計算單一欄位的 DET (Data Element Types) 數量
        /// IFPUG 規則：只計算有 PIC 的葉節點，OCCURS 不乘倍數
        /// </summary>
        /// <param name="field">COBOL 欄位</param>
        /// <returns>DET 數量</returns>
        public static int CountDET(CobolField field)
        {
            // 葉節點（有 PIC）→ 計為 1 DET
            if (field.IsLeafField)
                return 1;

            // 群組欄位 → 遞迴計算子項
            return field.Children.Sum(child => CountDET(child));
        }

        /// <summary>
        /// 批次計算所有 FD layout 的 DET
        /// </summary>
        /// <param name="cobolCode">COBOL 原始碼或 listing</param>
        /// <returns>字典：欄位名稱 → DET 數量</returns>
        public static Dictionary<string, int> CalculateFDDETs(string cobolCode)
        {
            var fields = ParseCobolStructure(cobolCode);
            var result = new Dictionary<string, int>();

            foreach (var field in fields)
            {
                result[field.Name] = CountDET(field);
            }
            return result;
        }

        #endregion

        #region Public API - Size 計算

        /// <summary>
        /// 計算單一欄位的 bytes 大小（遞迴，含 OCCURS 乘倍數）
        /// </summary>
        /// <param name="field">COBOL 欄位</param>
        /// <returns>bytes 大小</returns>
        public static int CountSize(CobolField field)
        {
            if (field.Children.Any())
            {
                int total = field.Children.Sum(child => CountSize(child));
                return total * field.Occurs;
            }
            else
            {
                return ComputePicSize(field) * field.Occurs;
            }
        }

        /// <summary>
        /// 批次計算所有 FD layout 的 bytes 大小
        /// </summary>
        /// <param name="cobolCode">COBOL 原始碼或 listing</param>
        /// <returns>字典：欄位名稱 → bytes 大小</returns>
        public static Dictionary<string, int> CalculateFDSizes(string cobolCode)
        {
            var fields = ParseCobolStructure(cobolCode);
            var result = new Dictionary<string, int>();

            foreach (var field in fields)
            {
                var flatField = FlattenField(field);
                int size = CountSize(flatField);
                result[field.Name] = size;
            }
            return result;
        }

        #endregion

        #region Public API - 解析

        /// <summary>
        /// 解析 COBOL 原始碼為欄位階層結構
        /// </summary>
        /// <param name="cobolCode">COBOL 原始碼或 listing</param>
        /// <returns>欄位列表</returns>
        public static List<CobolField> ParseCobolStructure(string cobolCode)
        {
            var fields = new List<CobolField>();
            var stack = new Stack<CobolField>();
            var lines = cobolCode.Split(new string[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);

            CobolField? currentFD = null;
            int skipLevel = int.MaxValue;

            foreach (var line in lines)
            {
                string trimmed = Regex.Replace(line.Trim(), @"\s+", " ").Replace("\u00A0", " ");

                // Skip comment lines
                if (trimmed.StartsWith("*>") || trimmed.StartsWith("*"))
                    continue;

                // FD Handling
                if (trimmed.StartsWith("FD", StringComparison.OrdinalIgnoreCase))
                {
                    var fdMatch = Regex.Match(trimmed, @"^FD\s+([\w-]+)", RegexOptions.IgnoreCase);
                    if (fdMatch.Success)
                    {
                        var fdField = new CobolField
                        {
                            Level = 0,
                            Name = fdMatch.Groups[1].Value
                        };
                        fields.Add(fdField);
                        currentFD = fdField;
                    }
                    continue;
                }

                // Process normal lines
                var levelMatch = Regex.Match(trimmed, @"^(0[1-9]|[1-4]\d|77)\s+([\w-]+)");
                if (!levelMatch.Success)
                    continue;

                int currentLevel = int.Parse(levelMatch.Groups[1].Value);

                // Skip REDEFINES blocks
                if (currentLevel > skipLevel)
                    continue;
                if (currentLevel <= skipLevel)
                    skipLevel = int.MaxValue;
                if (Regex.IsMatch(trimmed, @"\bREDEFINES\b", RegexOptions.IgnoreCase))
                {
                    skipLevel = currentLevel;
                    continue;
                }

                var field = new CobolField
                {
                    Level = currentLevel,
                    Name = levelMatch.Groups[2].Value
                };

                // Parse PIC clause
                var picMatch = Regex.Match(trimmed,
                    @"PIC\s+([S]?)([9X]+)(\((\d+)\))?(V[9]+(\((\d+)\))?)?",
                    RegexOptions.IgnoreCase);

                if (picMatch.Success)
                {
                    string signPrefix = picMatch.Groups[1].Value;
                    field.DataType = (signPrefix + picMatch.Groups[2].Value).Trim();

                    if (picMatch.Groups[4].Success)
                    {
                        field.Length = int.Parse(picMatch.Groups[4].Value);
                    }
                    else if (picMatch.Groups[2].Success)
                    {
                        field.Length = picMatch.Groups[2].Value.Length;
                    }

                    if (picMatch.Groups[5].Success)
                    {
                        if (picMatch.Groups[7].Success)
                        {
                            field.DecimalPlaces = int.Parse(picMatch.Groups[7].Value);
                        }
                        else
                        {
                            string decimalPart = picMatch.Groups[5].Value;
                            field.DecimalPlaces = decimalPart.Length - 1;
                        }
                        field.Length += field.DecimalPlaces;
                    }
                }

                // Parse OCCURS clause
                var occursMatch = Regex.Match(trimmed, @"OCCURS\s+(\d+)", RegexOptions.IgnoreCase);
                if (occursMatch.Success)
                {
                    field.Occurs = int.Parse(occursMatch.Groups[1].Value);
                }

                // FD Block Attachment
                if (currentFD != null && currentLevel == 1)
                {
                    currentFD.Children.Add(field);
                    stack.Clear();
                    stack.Push(field);
                    currentFD = null;
                    continue;
                }

                // Build hierarchy
                while (stack.Count > 0 && stack.Peek().Level >= field.Level)
                {
                    stack.Pop();
                }
                if (stack.Count > 0)
                {
                    stack.Peek().Children.Add(field);
                }
                else
                {
                    fields.Add(field);
                }
                if (field.Level < 49)
                    stack.Push(field);
            }
            return fields;
        }

        #endregion

        #region Private Helpers

        /// <summary>
        /// Flatten redundant nested groups
        /// </summary>
        private static CobolField FlattenField(CobolField field)
        {
            while (field.Children.Count == 1 &&
                   field.Name.Equals(field.Children[0].Name, StringComparison.OrdinalIgnoreCase))
            {
                var child = field.Children[0];
                child.Occurs *= field.Occurs;
                field = child;
            }
            for (int i = 0; i < field.Children.Count; i++)
            {
                field.Children[i] = FlattenField(field.Children[i]);
            }
            return field;
        }

        /// <summary>
        /// Compute PIC field size
        /// </summary>
        private static int ComputePicSize(CobolField field)
        {
            if (string.IsNullOrEmpty(field.DataType))
                return 0;

            return field.Length;
        }

        #endregion
    }
}

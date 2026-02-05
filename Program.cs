/*
 * 更新時間：2026-02-05 16:00
 * 作者：Cursor
 * 摘要：添加處理進度顯示功能，顯示當前進度、百分比和檔案名稱
 * 更新時間：2026-02-05 15:54
 * 作者：Cursor
 * 摘要：調整 CSV 輸出順序（名稱改為第一欄）、修正 DET=0 時輸出 0 並評估為 Low（不再輸出 "-"）
 * 更新時間：2026-02-05 15:05
 * 作者：Cursor
 * 摘要：遷移至 .NET Framework 4.8，移除 Encoding.RegisterProvider（.NET Framework 4.8 原生支援 Big5 編碼）
 * 更新時間：2026-02-04 19:45
 * 作者：Cursor
 * 摘要：--test 時優先使用與執行檔同目錄的 layout 資料夾，發佈至 publish 後新手可直接執行 --test。
 * 更新時間：2026-02-04 12:15
 * 作者：Cursor
 * 摘要：OutputFile 必填驗證與輸出目錄建立，確保使用 App.config 指定路徑建立 output.csv
 * 
 * 更新歷程：
 * - 2026-02-04 12:15: OutputFile 空值驗證、輸出目錄不存在時自動建立
 * - 2026-02-04 12:00: ExtractFDBlocks 開頭正規化 \r\n/\r→\n 並以 \n 分割，修正 HZ_ATLH.MST 等 Unix 行尾檔 DET=0
 * - 2026-02-04 11:20: 移除 EXTERNAL 檢查與命名；FilterExternalEntries→ExtractFDBlocks，CalculateExternal*→CalculateFD*
 * - 2026-02-04 11:15: 無 EXTERNAL 的 FD 亦以相同邏輯處理；擷取所有 FD、CobolLayoutAnalyzer 計算所有 FD
 * - 2026-02-04 11:05: 重構移除「檢查 EXTERNAL」邏輯；改為統一建 rowsToEmit 後單一迴圈輸出，log 僅 Processed/Error
 * - 2026-02-04 11:00: 移除「無 EXTERNAL 即跳過」；所有檔案皆產出至少一列
 * - 2026-02-03 19:45: 排除 output/log 被掃描、逐檔 log（Processed/SkippedNoExternal/Error）、寫檔 try/catch 與備援檔名
 * - 2026-02-03 19:20: 移除 EXTERNAL 項目檢查，無 EXTERNAL 項目的檔案靜默跳過
 * - 2026-02-03 19:15: 移除 .cbl 格式檢查，改用檔案路徑取得檔名，檔案修改時間作為日期
 * - 2026-02-03 18:30: 重構使用 CobolLayoutLib 共享庫，新增 DET/Complexity 計算
 * - 原始版本: 計算 EXTERNAL 欄位大小，輸出 CSV
 */
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Configuration;
using System.Text.RegularExpressions;
using CobolLayoutLib;

public class CobolFieldCalculator
{
    /// <summary>
    /// 從 COBOL 內容擷取 FD 區塊。
    /// </summary>
    public static string ExtractFDBlocks(string cobolListing)
    {
        // 行尾正規化：Unix(\n) / 舊 Mac(\r) 皆轉成 \n，避免整檔被當成一行導致 FD 區塊未擷取（DET=0）
        string normalized = cobolListing.Replace("\r\n", "\n").Replace("\r", "\n");
        char[] newLine = new[] { '\n' };

        var level78Mapping = new Dictionary<string, string>();
        Regex level78Pattern = new Regex(@"^(?!\s*(\*|>)).*?\s*78\s+(\S+).*?\bVALUE\s+(\d+)\b", RegexOptions.IgnoreCase);
        var allLines = normalized.Split(newLine, StringSplitOptions.None);
        
        foreach (var rawLine in allLines)
        {
            string normLine = Regex.Replace(rawLine, @"\s+", " ").Replace("\u00A0", " ");
            if (normLine.StartsWith(" *"))
            {
                continue;
            }

            if (normLine.Contains("78 "))
            {
                int levelIndex = normLine.IndexOf(" 78 ") + 4;
                int valueIndex = normLine.IndexOf("VALUE ", levelIndex);
                if (valueIndex == -1)
                    continue;

                string varName = normLine.Substring(levelIndex, valueIndex - levelIndex).Trim();
                if (string.IsNullOrWhiteSpace(varName)) continue;

                int numberStart = valueIndex + 6;
                int numberEnd = normLine.IndexOf(".", numberStart);
                if (numberEnd == -1) numberEnd = normLine.Length;

                string valuePart = normLine.Substring(numberStart, numberEnd - numberStart).Trim();
                if (!int.TryParse(valuePart, out _)) continue;

                level78Mapping[varName] = valuePart;
            }
        }

        var lines = normalized.Split(newLine, StringSplitOptions.None);
        var filteredBlocks = new List<string>();
        bool inBlock = false;
        int currentBlockLevel = 0;
        var currentBlockLines = new List<string>();

        Regex fdStartPattern = new Regex(@"^\s*FD\s+\S+", RegexOptions.IgnoreCase);
        Regex o1ExternalPattern = new Regex(@"^\s*01\s+\S+.*\bEXTERNAL\b", RegexOptions.IgnoreCase);
        Regex levelPattern = new Regex(@"^\s*(\d{2})\s+");

        foreach (var rawLine in lines)
        {
            string line = Regex.Replace(rawLine, @"\s+", " ").Replace("\u00A0", " ");
            if (line.Contains("Page:") || line.Contains("ACUCOBOL-GT") ||
                line.Contains("IDENTIFICATION DIVISION.") || line.Contains("STATISTICS") ||
                line.Contains("Code Size:") || line.Contains("Data Size:"))
            {
                continue;
            }

            if (fdStartPattern.IsMatch(line) || o1ExternalPattern.IsMatch(line))
            {
                if (inBlock && currentBlockLines.Any())
                {
                    string blockText = string.Join(Environment.NewLine, currentBlockLines);
                    foreach (var kvp in level78Mapping)
                    {
                        blockText = Regex.Replace(blockText,
                            $@"(OCCURS\s+){Regex.Escape(kvp.Key)}(\s+TIMES)",
                            "${1}" + kvp.Value + "${2}",
                            RegexOptions.IgnoreCase);
                    }
                    filteredBlocks.Add(blockText);
                    currentBlockLines.Clear();
                }
                inBlock = true;
                var levelMatch = levelPattern.Match(line);
                currentBlockLevel = levelMatch.Success ? int.Parse(levelMatch.Groups[1].Value) : 0;
                currentBlockLines.Add(line);
                continue;
            }

            if (inBlock)
            {
                var levelMatch = levelPattern.Match(line);
                if (levelMatch.Success)
                {
                    int level = int.Parse(levelMatch.Groups[1].Value);
                    if (level <= currentBlockLevel)
                    {
                        string blockText = string.Join(Environment.NewLine, currentBlockLines);
                        foreach (var kvp in level78Mapping)
                        {
                            blockText = Regex.Replace(blockText,
                                $@"(OCCURS\s+){kvp.Key}(\s+TIMES)",
                                "${1}" + kvp.Value + "${2}",
                                RegexOptions.IgnoreCase);
                        }
                        filteredBlocks.Add(blockText);
                        currentBlockLines.Clear();
                        inBlock = false;
                        if (fdStartPattern.IsMatch(line) || o1ExternalPattern.IsMatch(line))
                        {
                            inBlock = true;
                            currentBlockLevel = level;
                            currentBlockLines.Add(line);
                        }
                    }
                    else
                    {
                        currentBlockLines.Add(line);
                    }
                }
                else
                {
                    currentBlockLines.Add(line);
                }
            }
        }

        if (inBlock && currentBlockLines.Any())
        {
            string blockText = string.Join(Environment.NewLine, currentBlockLines);
            foreach (var kvp in level78Mapping)
            {
                blockText = Regex.Replace(blockText,
                    $@"(OCCURS\s+){kvp.Key}(\s+TIMES)",
                    "${1}" + kvp.Value + "${2}",
                    RegexOptions.IgnoreCase);
            }
            filteredBlocks.Add(blockText);
        }
        return string.Join(Environment.NewLine + Environment.NewLine, filteredBlocks);
    }

    /// <summary>
    /// Main entry point - reads COBOL listings and outputs CSV with Size, DET, and Complexity
    /// Usage: cobolExternal [--test]
    /// </summary>
    public static void Main(string[] args)
    {
        // Test mode: analyze layout folder samples
        if (args.Length > 0 && args[0] == "--test")
        {
            RunLayoutTest();
            return;
        }

        // Test single file mode: verify DET calculation for specific file
        if (args.Length > 0 && args[0] == "--verify" && args.Length > 1)
        {
            VerifyFileDET(args[1]);
            return;
        }

        string sourceFolder = ConfigurationManager.AppSettings["SourceFolder"];
        string outputFile = ConfigurationManager.AppSettings["OutputFile"];
        string fileExtension = ConfigurationManager.AppSettings["FileExtension"] ?? "*.lst";

        if (string.IsNullOrEmpty(sourceFolder) || !Directory.Exists(sourceFolder))
        {
            Console.WriteLine("Source folder not found or not specified in app.config.");
            return;
        }

        if (string.IsNullOrEmpty(outputFile))
        {
            Console.WriteLine("OutputFile not specified in app.config. Add: <add key=\"OutputFile\" value=\"C:\\path\\to\\output.csv\" />");
            return;
        }

        string[] allFiles = Directory.GetFiles(sourceFolder, fileExtension);
        string outputFullPath = Path.GetFullPath(outputFile);
        string outputDir = Path.GetDirectoryName(outputFullPath);
        if (!string.IsNullOrEmpty(outputDir) && !Directory.Exists(outputDir))
        {
            Directory.CreateDirectory(outputDir);
        }
        string logFileName = "cobolfd_" + DateTime.Now.ToString("yyyyMMdd_HHmmss") + ".log";
        string logFullPath = Path.Combine(Path.GetDirectoryName(outputFullPath) ?? sourceFolder, logFileName);

        var files = allFiles.Where(f =>
        {
            string full = Path.GetFullPath(f);
            if (string.Equals(full, outputFullPath, StringComparison.OrdinalIgnoreCase))
                return false;
            if (string.Equals(Path.GetExtension(f), ".log", StringComparison.OrdinalIgnoreCase))
                return false;
            return true;
        }).ToArray();

        if (files.Length == 0)
        {
            Console.WriteLine($"No files to process (after excluding output and log). Folder: {sourceFolder}, Extension: {fileExtension}.");
            return;
        }

        // .NET Framework 4.8 原生支援 Big5 編碼，不需要註冊 Provider
        Encoding big5 = Encoding.GetEncoding("big5");

        var csvLines = new List<string> { "名稱,檔名,日期,大小,DET,複雜度" };
        int processedCount = 0;
        int errorCount = 0;
        string actualOutputPath = outputFullPath;

        using (var logWriter = new StreamWriter(logFullPath, false, Encoding.UTF8))
        {
            logWriter.WriteLine($"=== CobolFD Run Log (Taiwan UTC+8) ===");
            logWriter.WriteLine($"StartTime={DateTime.Now:yyyy-MM-dd HH:mm:ss}");
            logWriter.WriteLine($"SourceFolder={sourceFolder}");
            logWriter.WriteLine($"FileExtension={fileExtension}");
            logWriter.WriteLine($"OutputFile={outputFullPath}");
            logWriter.WriteLine($"ScannedFilesTotal={allFiles.Length}");
            logWriter.WriteLine($"Excluded=output file and *.log");
            logWriter.WriteLine($"FilesToProcess={files.Length}");
            logWriter.WriteLine();

            int totalFiles = files.Length;
            Console.WriteLine($"處理檔案: {totalFiles} 個檔案");

            for (int i = 0; i < files.Length; i++)
            {
                string filePath = files[i];
                string fileName = Path.GetFileName(filePath);
                string timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");
                int currentIndex = i + 1;
                double percentage = (double)currentIndex / totalFiles * 100;

                // 顯示處理中
                Console.Write($"\r[{currentIndex}/{totalFiles}] ({percentage:F1}%) Processing: {fileName}...");

                try
                {
                    string cobolListing = File.ReadAllText(filePath, big5);
                    string fdBlocks = ExtractFDBlocks(cobolListing);
                    var fdSizes = CobolLayoutAnalyzer.CalculateFDSizes(fdBlocks);
                    var fdDETs = CobolLayoutAnalyzer.CalculateFDDETs(fdBlocks);

                    string programName = Path.GetFileName(filePath);
                    string fileDate = File.GetLastWriteTime(filePath).ToString("yyyy-MM-dd HH:mm:ss");

                    var rowsToEmit = new List<(string name, int size, int det)>();
                    foreach (var kvp in fdSizes)
                        rowsToEmit.Add((kvp.Key, kvp.Value, fdDETs.ContainsKey(kvp.Key) ? fdDETs[kvp.Key] : 0));
                    if (rowsToEmit.Count == 0)
                        rowsToEmit.Add(("-", 0, 0));

                    foreach (var r in rowsToEmit)
                    {
                        // DET=0 時也進行複雜度評估（歸類為 Low），不再輸出 "-"
                        string complexity = ComplexityEvaluator.EvaluateComplexity(r.det);
                        // CSV 欄位順序：名稱,檔名,日期,大小,DET,複雜度
                        csvLines.Add($"{r.name},{programName},{fileDate},{r.size},{r.det},{complexity}");
                    }

                    processedCount++;
                    logWriter.WriteLine($"[{timestamp}] [Processed] file={fileName} rows={rowsToEmit.Count}");
                    
                    // 顯示完成（使用空格清除多餘字元）
                    Console.Write($"\r[{currentIndex}/{totalFiles}] ({percentage:F1}%) Completed: {fileName}    ");
                }
                catch (Exception ex)
                {
                    errorCount++;
                    logWriter.WriteLine($"[{timestamp}] [Error] file={fileName} message={ex.Message}");
                    logWriter.WriteLine($"  StackTrace: {ex.StackTrace}");
                    
                    // 顯示錯誤（使用空格清除多餘字元）
                    Console.Write($"\r[{currentIndex}/{totalFiles}] ({percentage:F1}%) Error: {fileName}    ");
                }
            }

            // 處理完成後換行
            Console.WriteLine();

            try
            {
                File.WriteAllLines(actualOutputPath, csvLines, new UTF8Encoding(true));
            }
            catch (IOException ex)
            {
                string fallback = Path.Combine(Path.GetDirectoryName(outputFullPath) ?? sourceFolder,
                    "output_" + DateTime.Now.ToString("yyyyMMdd_HHmmss") + ".csv");
                try
                {
                    File.WriteAllLines(fallback, csvLines, new UTF8Encoding(true));
                    actualOutputPath = fallback;
                    logWriter.WriteLine($"[{DateTime.Now:yyyy-MM-dd HH:mm:ss}] [WriteOutputFailed] primary={outputFullPath} reason={ex.Message}");
                    logWriter.WriteLine($"  Fallback written to: {fallback}");
                }
                catch (Exception ex2)
                {
                    logWriter.WriteLine($"[{DateTime.Now:yyyy-MM-dd HH:mm:ss}] [WriteOutputFailed] primary={outputFullPath} reason={ex.Message}");
                    logWriter.WriteLine($"  Fallback also failed: {ex2.Message}");
                }
            }

            logWriter.WriteLine();
            logWriter.WriteLine("=== Summary ===");
            logWriter.WriteLine($"TotalFiles={files.Length}");
            logWriter.WriteLine($"Processed={processedCount}");
            logWriter.WriteLine($"Error={errorCount}");
            logWriter.WriteLine($"TotalOutputRows={csvLines.Count - 1}");
            logWriter.WriteLine($"OutputPath={actualOutputPath}");
        }

        Console.WriteLine($"CSV output written to {actualOutputPath}");
        Console.WriteLine($"Log written to {logFullPath}");
        Console.WriteLine($"Summary: Processed={processedCount}, Error={errorCount}, TotalRows={csvLines.Count - 1}");
        Console.WriteLine($"Complexity thresholds: {ComplexityEvaluator.GetThresholdDescription()}");
    }

    /// <summary>
    /// Test mode: analyze layout folder samples for DET calculation verification
    /// </summary>
    static void RunLayoutTest()
    {
        Console.WriteLine("=== CobolLayoutLib DET 計算測試 ===\n");
        Console.WriteLine($"複雜度閾值：{ComplexityEvaluator.GetThresholdDescription()}\n");

        // 優先使用與執行檔同目錄的 layout（發佈後新手可直接 --test）
        string layoutFolder = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "layout");
        if (!Directory.Exists(layoutFolder))
        {
            layoutFolder = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "..", "..", "..", "layout");
        }
        if (!Directory.Exists(layoutFolder))
        {
            layoutFolder = Path.Combine(Path.GetDirectoryName(AppDomain.CurrentDomain.BaseDirectory.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)) ?? "", "layout");
        }

        if (!Directory.Exists(layoutFolder))
        {
            Console.WriteLine($"Layout folder not found: {layoutFolder}");
            return;
        }

        string[] mstFiles = Directory.GetFiles(layoutFolder, "*.mst");
        
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        Encoding big5 = Encoding.GetEncoding("big5");

        Console.WriteLine($"找到 {mstFiles.Length} 個 .mst 檔案\n");

        foreach (var filePath in mstFiles)
        {
            string fileName = Path.GetFileName(filePath);
            string content = File.ReadAllText(filePath, big5);

            var dets = CobolLayoutAnalyzer.CalculateFDDETs(content);
            var sizes = CobolLayoutAnalyzer.CalculateFDSizes(content);

            Console.WriteLine($"檔案: {fileName}");
            Console.WriteLine(new string('-', 50));

            foreach (var kvp in dets)
            {
                int det = kvp.Value;
                int size = sizes.ContainsKey(kvp.Key) ? sizes[kvp.Key] : 0;
                string complexity = ComplexityEvaluator.EvaluateComplexity(det);
                int ilfWeight = ComplexityEvaluator.GetIlfWeight(complexity);
                int eifWeight = ComplexityEvaluator.GetEifWeight(complexity);

                Console.WriteLine($"  {kvp.Key}:");
                Console.WriteLine($"    Size: {size} bytes");
                Console.WriteLine($"    DET:  {det}");
                Console.WriteLine($"    Complexity: {complexity}");
                Console.WriteLine($"    ILF Weight: {ilfWeight}, EIF Weight: {eifWeight}");
            }
            Console.WriteLine();
        }

        Console.WriteLine("=== 測試完成 ===");
    }

    /// <summary>
    /// 驗證單一檔案的 DET 計算
    /// </summary>
    static void VerifyFileDET(string filePath)
    {
        if (!File.Exists(filePath))
        {
            Console.WriteLine($"檔案不存在: {filePath}");
            return;
        }

        // .NET Framework 4.8 原生支援 Big5 編碼，不需要註冊 Provider
        Encoding big5 = Encoding.GetEncoding("big5");
        string content = File.ReadAllText(filePath, big5);

        var dets = CobolLayoutAnalyzer.CalculateFDDETs(content);
        var sizes = CobolLayoutAnalyzer.CalculateFDSizes(content);
        var fields = CobolLayoutAnalyzer.ParseCobolStructure(content);

        Console.WriteLine($"=== {Path.GetFileName(filePath)} DET 驗證 ===\n");

        foreach (var kvp in dets)
        {
            int det = kvp.Value;
            int size = sizes.ContainsKey(kvp.Key) ? sizes[kvp.Key] : 0;

            Console.WriteLine($"欄位名稱: {kvp.Key}");
            Console.WriteLine($"計算的 DET: {det}");
            Console.WriteLine($"計算的 Size: {size} bytes");
            Console.WriteLine();
        }

        // 列出所有葉節點欄位
        Console.WriteLine("=== 所有葉節點欄位列表（用於 DET 計算）===");
        int totalDET = 0;
        foreach (var field in fields)
        {
            totalDET = CountLeafFields(field, "", 0);
        }
        Console.WriteLine($"\n總計 DET: {totalDET}");
    }

    static int CountLeafFields(CobolField field, string indent, int count)
    {
        if (field.IsLeafField)
        {
            count++;
            Console.WriteLine($"{indent}{count}. {field.Name} (Level {field.Level}, PIC {field.DataType}, Length {field.Length})");
        }
        else
        {
            if (!string.IsNullOrEmpty(field.Name) && field.Level > 0)
                Console.WriteLine($"{indent}{field.Name} (Level {field.Level}, Group)");
            foreach (var child in field.Children)
            {
                count = CountLeafFields(child, indent + "  ", count);
            }
        }
        return count;
    }
}

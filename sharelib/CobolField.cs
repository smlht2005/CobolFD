/*
 * 更新時間：2026-02-03 18:30
 * 作者：Cursor
 * 摘要：新增 CobolField 欄位資料模型，從 Program.cs 抽取為共享類別
 */
using System.Collections.Generic;

namespace CobolLayoutLib
{
    /// <summary>
    /// COBOL 欄位資料結構
    /// 用於表示 COBOL DATA DIVISION 中的欄位階層
    /// </summary>
    public class CobolField
    {
        /// <summary>層級編號 (01-49, 77, 0=FD)</summary>
        public int Level { get; set; }

        /// <summary>欄位名稱</summary>
        public string Name { get; set; } = string.Empty;

        /// <summary>PIC 資料類型 (9, X, S9, etc.)</summary>
        public string DataType { get; set; } = string.Empty;

        /// <summary>欄位長度 (bytes)</summary>
        public int Length { get; set; }

        /// <summary>小數位數</summary>
        public int DecimalPlaces { get; set; }

        /// <summary>OCCURS 次數 (預設 1)</summary>
        public int Occurs { get; set; } = 1;

        /// <summary>子欄位集合</summary>
        public List<CobolField> Children { get; set; } = new List<CobolField>();

        /// <summary>
        /// 是否為葉節點（有 PIC 定義且無子欄位）
        /// </summary>
        public bool IsLeafField => !string.IsNullOrEmpty(DataType) && Children.Count == 0;

        /// <summary>
        /// 是否為群組欄位（無 PIC 定義但有子欄位）
        /// </summary>
        public bool IsGroupField => string.IsNullOrEmpty(DataType) && Children.Count > 0;
    }
}

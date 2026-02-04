/*
 * 更新時間：2026-02-03 18:30
 * 作者：Cursor
 * 摘要：新增 ComplexityEvaluator 複雜度評估類別，根據 IFPUG 標準判定 Low/Average/High
 */
namespace CobolLayoutLib
{
    /// <summary>
    /// IFPUG 複雜度評估
    /// 提供 DET 到複雜度的轉換及 Function Point 權重計算
    /// </summary>
    public static class ComplexityEvaluator
    {
        #region 複雜度閾值常數

        /// <summary>Low 複雜度上限</summary>
        public const int LowThreshold = 5;

        /// <summary>Average 複雜度上限</summary>
        public const int AverageThreshold = 15;

        #endregion

        #region 複雜度評估

        /// <summary>
        /// 根據 DET 評估 ILF/EIF 複雜度
        /// </summary>
        /// <param name="det">Data Element Types 數量</param>
        /// <returns>Low / Average / High</returns>
        public static string EvaluateComplexity(int det)
        {
            if (det <= LowThreshold) return "Low";
            if (det <= AverageThreshold) return "Average";
            return "High";
        }

        /// <summary>
        /// 根據 DET 評估複雜度（回傳列舉）
        /// </summary>
        public static ComplexityLevel GetComplexityLevel(int det)
        {
            if (det <= LowThreshold) return ComplexityLevel.Low;
            if (det <= AverageThreshold) return ComplexityLevel.Average;
            return ComplexityLevel.High;
        }

        #endregion

        #region Function Point 權重

        /// <summary>
        /// 複雜度對應的 Function Point 權重（ILF - Internal Logical File）
        /// </summary>
        public static int GetIlfWeight(string complexity)
        {
            return complexity switch
            {
                "Low" => 7,
                "Average" => 10,
                "High" => 15,
                _ => 10
            };
        }

        /// <summary>
        /// 複雜度對應的 Function Point 權重（ILF - Internal Logical File）
        /// </summary>
        public static int GetIlfWeight(ComplexityLevel level)
        {
            return level switch
            {
                ComplexityLevel.Low => 7,
                ComplexityLevel.Average => 10,
                ComplexityLevel.High => 15,
                _ => 10
            };
        }

        /// <summary>
        /// 複雜度對應的 Function Point 權重（EIF - External Interface File）
        /// </summary>
        public static int GetEifWeight(string complexity)
        {
            return complexity switch
            {
                "Low" => 5,
                "Average" => 7,
                "High" => 10,
                _ => 7
            };
        }

        /// <summary>
        /// 複雜度對應的 Function Point 權重（EIF - External Interface File）
        /// </summary>
        public static int GetEifWeight(ComplexityLevel level)
        {
            return level switch
            {
                ComplexityLevel.Low => 5,
                ComplexityLevel.Average => 7,
                ComplexityLevel.High => 10,
                _ => 7
            };
        }

        #endregion

        #region 輔助方法

        /// <summary>
        /// 取得複雜度閾值說明
        /// </summary>
        public static string GetThresholdDescription()
        {
            return $"Low: DET ≤ {LowThreshold}, Average: {LowThreshold} < DET ≤ {AverageThreshold}, High: DET > {AverageThreshold}";
        }

        #endregion
    }

    /// <summary>
    /// 複雜度等級列舉
    /// </summary>
    public enum ComplexityLevel
    {
        Low,
        Average,
        High
    }
}

/*
 * 更新時間：2026-02-05 15:15
 * 作者：Cursor
 * 摘要：重構從 App.config 讀取複雜度閾值，移除硬編碼常數，支援動態設定
 * 更新時間：2026-02-03 18:30
 * 作者：Cursor
 * 摘要：新增 ComplexityEvaluator 複雜度評估類別，根據 IFPUG 標準判定 Low/Average/High
 */
using System;
using System.Configuration;

namespace CobolLayoutLib
{
    /// <summary>
    /// IFPUG 複雜度評估
    /// 提供 DET 到複雜度的轉換及 Function Point 權重計算
    /// </summary>
    public static class ComplexityEvaluator
    {
        #region 複雜度閾值（從 App.config 讀取）

        /// <summary>
        /// 從 App.config 讀取 Low 複雜度上限
        /// 預設值：19（DET ≤ 19 為 Low）
        /// </summary>
        private static int GetLowThreshold()
        {
            string value = ConfigurationManager.AppSettings["ComplexityLowThreshold"];
            if (string.IsNullOrEmpty(value))
                return 19; // 預設值
            return int.Parse(value);
        }

        /// <summary>
        /// 從 App.config 讀取 Average 複雜度上限
        /// 預設值：50（20 ≤ DET ≤ 50 為 Average，DET > 50 為 High）
        /// </summary>
        private static int GetAverageThreshold()
        {
            string value = ConfigurationManager.AppSettings["ComplexityAverageThreshold"];
            if (string.IsNullOrEmpty(value))
                return 50; // 預設值
            return int.Parse(value);
        }

        #endregion

        #region 複雜度評估

        /// <summary>
        /// 根據 DET 評估 ILF/EIF 複雜度
        /// </summary>
        /// <param name="det">Data Element Types 數量</param>
        /// <returns>Low / Average / High</returns>
        public static string EvaluateComplexity(int det)
        {
            int lowThreshold = GetLowThreshold();
            int avgThreshold = GetAverageThreshold();
            
            // DET = 0 也歸類為 Low（DET ≤ lowThreshold）
            if (det <= lowThreshold) return "Low";
            if (det <= avgThreshold) return "Average";
            return "High";  // DET > avgThreshold
        }

        /// <summary>
        /// 根據 DET 評估複雜度（回傳列舉）
        /// </summary>
        public static ComplexityLevel GetComplexityLevel(int det)
        {
            int lowThreshold = GetLowThreshold();
            int avgThreshold = GetAverageThreshold();
            
            if (det <= lowThreshold) return ComplexityLevel.Low;
            if (det <= avgThreshold) return ComplexityLevel.Average;
            return ComplexityLevel.High;
        }

        #endregion

        #region Function Point 權重

        /// <summary>
        /// 複雜度對應的 Function Point 權重（ILF - Internal Logical File）
        /// </summary>
        public static int GetIlfWeight(string complexity)
        {
            switch (complexity)
            {
                case "Low": return 7;
                case "Average": return 10;
                case "High": return 15;
                default: return 10;
            }
        }

        /// <summary>
        /// 複雜度對應的 Function Point 權重（ILF - Internal Logical File）
        /// </summary>
        public static int GetIlfWeight(ComplexityLevel level)
        {
            switch (level)
            {
                case ComplexityLevel.Low: return 7;
                case ComplexityLevel.Average: return 10;
                case ComplexityLevel.High: return 15;
                default: return 10;
            }
        }

        /// <summary>
        /// 複雜度對應的 Function Point 權重（EIF - External Interface File）
        /// </summary>
        public static int GetEifWeight(string complexity)
        {
            switch (complexity)
            {
                case "Low": return 5;
                case "Average": return 7;
                case "High": return 10;
                default: return 7;
            }
        }

        /// <summary>
        /// 複雜度對應的 Function Point 權重（EIF - External Interface File）
        /// </summary>
        public static int GetEifWeight(ComplexityLevel level)
        {
            switch (level)
            {
                case ComplexityLevel.Low: return 5;
                case ComplexityLevel.Average: return 7;
                case ComplexityLevel.High: return 10;
                default: return 7;
            }
        }

        #endregion

        #region 輔助方法

        /// <summary>
        /// 取得複雜度閾值說明
        /// </summary>
        public static string GetThresholdDescription()
        {
            int lowThreshold = GetLowThreshold();
            int avgThreshold = GetAverageThreshold();
            return $"Low: DET ≤ {lowThreshold}, Average: {lowThreshold} < DET ≤ {avgThreshold}, High: DET > {avgThreshold}";
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

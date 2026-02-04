# CobolFD DET 複雜度計算重構計畫（sharelib 版）

> 更新時間：2026-02-03 18:30
> 作者：Cursor
> 摘要：新增 sharelib 共享庫計算 COBOL layout 欄位數量 (DET)，根據 IFPUG 標準評估 ILF/EIF 複雜度

## 一、架構設計

### 新專案結構

```
CobolFD/
├── sharelib/                          <-- 新增共享類別庫
│   ├── CobolLayoutLib.csproj          <-- Class Library 專案
│   ├── CobolField.cs                  <-- 欄位資料模型
│   ├── CobolLayoutAnalyzer.cs         <-- 解析 + DET/Size 計算
│   └── ComplexityEvaluator.cs         <-- 複雜度評估
├── Program.cs                         <-- 主程式（引用 sharelib）
├── cobolExternal.csproj               <-- 更新：加入 sharelib 參考
└── ...
```

### 設計原則

- **sharelib 為純類別庫**：不依賴 App.config，可被任何專案引用
- **公開 API 設計**：所有計算方法為 `public static`，方便直接呼叫
- **參考現有邏輯**：`CountDET()` 參考 `ComputeSize()` 的遞迴模式

---

## 二、IFPUG DET 計算規則

| 條件 | 計算方式 |
|------|----------|
| 有 PIC 的葉節點 | 計為 1 DET |
| 群組欄位（無 PIC） | 不計算，遍歷子項 |
| REDEFINES | 跳過（已實作） |
| OCCURS N | 視為單一 DET（不乘 N） |

---

## 三、sharelib 公開 API

| 類別 | 方法 | 說明 |
|------|------|------|
| `CobolLayoutAnalyzer` | `ParseCobolStructure()` | 解析 COBOL 為欄位階層 |
| `CobolLayoutAnalyzer` | `CountDET()` | 計算 DET（葉節點數，不乘 OCCURS） |
| `CobolLayoutAnalyzer` | `CountSize()` | 計算 bytes 大小（乘 OCCURS） |
| `CobolLayoutAnalyzer` | `CalculateExternalDETs()` | 批次計算所有 EXTERNAL 的 DET |
| `CobolLayoutAnalyzer` | `CalculateExternalSizes()` | 批次計算所有 EXTERNAL 的 bytes |
| `ComplexityEvaluator` | `EvaluateComplexity()` | DET → Low/Average/High |
| `ComplexityEvaluator` | `GetIlfWeight()` | 複雜度 → ILF 權重 |
| `ComplexityEvaluator` | `GetEifWeight()` | 複雜度 → EIF 權重 |

---

## 四、複雜度閾值

| 複雜度 | DET 範圍 | ILF 權重 | EIF 權重 |
|--------|----------|----------|----------|
| Low | DET ≤ 5 | 7 | 5 |
| Average | 5 < DET ≤ 15 | 10 | 7 |
| High | DET > 15 | 15 | 10 |

---

## 五、CSV 輸出格式

```csv
程式名稱,List 日期,External 變數,大小,DET,Complexity
HZXXX.cbl,2026-02-03,HZPK3-MST,228,12,Average
```

---

## 六、檔案異動清單

- `CobolFD/sharelib/CobolLayoutLib.csproj`：新增 Class Library 專案
- `CobolFD/sharelib/CobolField.cs`：新增欄位模型
- `CobolFD/sharelib/CobolLayoutAnalyzer.cs`：新增解析與計算 API
- `CobolFD/sharelib/ComplexityEvaluator.cs`：新增複雜度評估
- `CobolFD/cobolExternal.csproj`：新增 sharelib 專案參考
- `CobolFD/Program.cs`：簡化，呼叫 sharelib API

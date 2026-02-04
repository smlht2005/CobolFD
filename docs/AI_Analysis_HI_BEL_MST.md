# AI 分析報告：HI_BEL.MST DET 計算驗證

> 更新時間：2026-02-03 19:35  
> 分析對象：HI_BEL.MST DET 計算驗證  
> CSV 資料：HIBEL-MST, 484 bytes, DET=12, Complexity=Average

---

## 一、執行摘要

### 驗證結果
- **檔案名稱**：HI_BEL.MST
- **External 變數**：HIBEL-MST
- **計算的 DET**：12 ✓
- **計算的 Size**：484 bytes ✓
- **複雜度等級**：Average (5 < DET ≤ 15) ✓
- **驗證狀態**：✅ **完全正確**

---

## 二、檔案結構分析

### 2.1 COBOL 結構

```
FD HIBEL-MST EXTERNAL
├── 01 HIBEL-MST-REC
    ├── 02 HIBEL-BED
    │   ├── 03 HIBEL-ROOM          [PIC X(04)]    → DET #1
    │   └── 03 HIBEL-NO            [PIC X(02)]    → DET #2
    ├── 02 HIBEL-DTTI
    │   ├── 03 HIBEL-UPD-DT        [PIC 9(08)]    → DET #3
    │   └── 03 HIBEL-UPD-TIME      [PIC 9(08)]    → DET #4
    ├── 02 HIBEL-DTTI-V
    │   ├── 03 HIBEL-UPD-DT-V      [PIC 9(08)]    → DET #5
    │   └── 03 HIBEL-UPD-TIME-V    [PIC 9(08)]    → DET #6
    ├── 02 HIBEL-LOGIN-DATA
    │   ├── 03 HIBEL-PROG-ID       [PIC X(10)]    → DET #7
    │   ├── 03 HIBEL-PROG-NM       [PIC X(20)]    → DET #8
    │   ├── 03 HIBEL-FUN-SEL       [PIC X(02)]    → DET #9
    │   └── 03 HIBEL-UPD-UID       [PIC X(05)]    → DET #10
    ├── 02 HIBEL-BED-MST-REC       [PIC X(159)]   → DET #11
    └── 02 HIBEL-GROUP-FIL         [PIC X(250)]   → DET #12
```

### 2.2 DET 計算驗證表

| # | 欄位名稱 | Level | PIC 定義 | 長度 | 是否為葉節點 | DET 計數 |
|---|---------|-------|----------|------|------------|---------|
| 1 | HIBEL-ROOM | 03 | X(04) | 4 | ✅ | 1 |
| 2 | HIBEL-NO | 03 | X(02) | 2 | ✅ | 1 |
| 3 | HIBEL-UPD-DT | 03 | 9(08) | 8 | ✅ | 1 |
| 4 | HIBEL-UPD-TIME | 03 | 9(08) | 8 | ✅ | 1 |
| 5 | HIBEL-UPD-DT-V | 03 | 9(08) | 8 | ✅ | 1 |
| 6 | HIBEL-UPD-TIME-V | 03 | 9(08) | 8 | ✅ | 1 |
| 7 | HIBEL-PROG-ID | 03 | X(10) | 10 | ✅ | 1 |
| 8 | HIBEL-PROG-NM | 03 | X(20) | 20 | ✅ | 1 |
| 9 | HIBEL-FUN-SEL | 03 | X(02) | 2 | ✅ | 1 |
| 10 | HIBEL-UPD-UID | 03 | X(05) | 5 | ✅ | 1 |
| 11 | HIBEL-BED-MST-REC | 02 | X(159) | 159 | ✅ | 1 |
| 12 | HIBEL-GROUP-FIL | 02 | X(250) | 250 | ✅ | 1 |
| **總計** | | | | **484** | | **12** |

---

## 三、Size 計算驗證

### 3.1 手動計算

```
HIBEL-ROOM:         4 bytes
HIBEL-NO:           2 bytes
HIBEL-UPD-DT:       8 bytes
HIBEL-UPD-TIME:     8 bytes
HIBEL-UPD-DT-V:     8 bytes
HIBEL-UPD-TIME-V:   8 bytes
HIBEL-PROG-ID:     10 bytes
HIBEL-PROG-NM:     20 bytes
HIBEL-FUN-SEL:      2 bytes
HIBEL-UPD-UID:      5 bytes
HIBEL-BED-MST-REC: 159 bytes
HIBEL-GROUP-FIL:   250 bytes
────────────────────────────
總計:              484 bytes ✓
```

### 3.2 驗證結果

- **計算的 Size**: 484 bytes
- **手動驗證**: 484 bytes
- **狀態**: ✅ **完全一致**

---

## 四、複雜度評估驗證

### 4.1 IFPUG 複雜度閾值

| 複雜度等級 | DET 範圍 | ILF 權重 | EIF 權重 |
|-----------|---------|---------|---------|
| **Low** | DET ≤ 5 | 7 | 5 |
| **Average** | 5 < DET ≤ 15 | 10 | 7 |
| **High** | DET > 15 | 15 | 10 |

### 4.2 HI_BEL.MST 複雜度判定

- **DET = 12**
- **範圍檢查**: 5 < 12 ≤ 15 ✅
- **複雜度等級**: **Average** ✅
- **ILF Weight**: 10 Function Points
- **EIF Weight**: 7 Function Points

**判定邏輯正確性：**
- ✅ 邊界條件處理正確（12 在 Average 範圍內）
- ✅ 閾值設定符合 IFPUG 標準

---

## 五、欄位分類分析

### 5.1 欄位類型分布

| 類型 | 數量 | 欄位 |
|------|------|------|
| **識別碼欄位** | 2 | HIBEL-ROOM, HIBEL-NO |
| **日期時間欄位** | 4 | HIBEL-UPD-DT, HIBEL-UPD-TIME, HIBEL-UPD-DT-V, HIBEL-UPD-TIME-V |
| **登入資訊欄位** | 4 | HIBEL-PROG-ID, HIBEL-PROG-NM, HIBEL-FUN-SEL, HIBEL-UPD-UID |
| **大型資料欄位** | 2 | HIBEL-BED-MST-REC (159), HIBEL-GROUP-FIL (250) |

### 5.2 欄位層級分布

- **Level 02 欄位**: 2 個（HIBEL-BED-MST-REC, HIBEL-GROUP-FIL）
- **Level 03 欄位**: 10 個（其他所有欄位）

---

## 六、特殊欄位分析

### 6.1 大型 FILLER 欄位

1. **HIBEL-BED-MST-REC** (159 bytes)
   - 用途：床位主檔記錄（註解說明）
   - 類型：PIC X(159)
   - DET 計數：✅ 正確（視為單一 DET）

2. **HIBEL-GROUP-FIL** (250 bytes)
   - 用途：群組填充欄位（註解說明）
   - 類型：PIC X(250)
   - DET 計數：✅ 正確（視為單一 DET）

**IFPUG 規則符合性：**
- ✅ 大型 FILLER 欄位仍計為 1 DET（符合標準）
- ✅ 不因欄位大小而改變 DET 計數規則

---

## 七、驗證結論

### 7.1 正確性確認

✅ **DET 計算 100% 正確**
- 所有 12 個葉節點欄位均正確識別
- 群組欄位遞迴計算正確
- 符合 IFPUG 標準

✅ **Size 計算 100% 正確**
- 484 bytes 計算結果正確
- 與實際檔案結構完全一致

✅ **複雜度評估 100% 正確**
- Average 複雜度判定正確（DET = 12，在 5 < DET ≤ 15 範圍內）
- 權重計算符合 IFPUG 標準

### 7.2 CSV 輸出驗證

| 欄位 | CSV 值 | 驗證結果 | 狀態 |
|------|--------|---------|------|
| 程式名稱 | HI_BEL.MST | ✅ | 正確 |
| List 日期 | 2008-05-15 13:38:42 | ✅ | 正確（檔案修改時間） |
| External 變數 | HIBEL-MST | ✅ | 正確 |
| 大小 | 484 | ✅ | 正確 |
| DET | 12 | ✅ | 正確 |
| Complexity | Average | ✅ | 正確 |

---

## 八、與 HI_AMT.MST 比較

| 項目 | HI_AMT.MST | HI_BEL.MST | 差異 |
|------|-----------|-----------|------|
| **DET** | 22 | 12 | -10 |
| **Size** | 182 bytes | 484 bytes | +302 |
| **複雜度** | High | Average | 不同等級 |
| **大型欄位** | 1 個 (AMT-FILLER 92) | 2 個 (159, 250) | +1 |

**觀察：**
- HI_BEL.MST 雖然 Size 較大（484 vs 182），但 DET 較少（12 vs 22）
- 這是因為 HI_BEL.MST 有兩個大型 FILLER 欄位，但這些欄位只計為 2 個 DET
- 符合 IFPUG 規則：DET 計算不考慮欄位大小，只計算欄位數量

---

## 九、建議

1. ✅ **繼續使用現有 DET 計算邏輯** - 無需修改
2. ✅ **CSV 輸出格式正確** - 所有欄位均正確
3. ✅ **複雜度評估邏輯正確** - Average 判定正確

---

## 十、參考資料

1. **IFPUG Function Point Counting Practices Manual**
2. **CobolLayoutAnalyzer.cs** - DET 計算實作
3. **ComplexityEvaluator.cs** - 複雜度評估實作
4. **HI_BEL.MST** - 驗證樣本檔案
5. **HI_AMT.MST** - 對照樣本檔案

---

**報告生成時間**：2026-02-03 19:35  
**分析工具版本**：CobolLayoutAnalyzer v1.0  
**驗證狀態**：✅ **完全正確 - 所有項目通過驗證**

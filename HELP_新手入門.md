# CobolFD — COBOL FD 區塊 DET/Size 工具 新手入門

> 更新時間：2026-02-05  
> 適用對象：第一次使用本工具的使用者  
> 運行環境：.NET Framework 4.8

---

## 一、這個工具在做什麼？

CobolFD 會掃描 **COBOL 來源檔**（.lst、.mst、.trn 等），擷取 **FD 區塊**，計算每個 FD 的：

- **Size**（位元組）
- **DET**（Data Element Types，資料元素類型數）
- **複雜度**（Low / Average / High）

並產出 **CSV 報表**，供功能點分析或後續估算使用。

---

## 二、你需要準備什麼？

1. **Windows 電腦**（.NET Framework 4.8 可執行檔，需已安裝 .NET Framework 4.8）
2. **COBOL 列表檔**：編譯產生的 .lst，或 .mst / .trn 等含 FD 區塊的文字檔（建議 Big5 編碼）

---

## 三、第一次使用：三步驟

### 步驟 1：設定 config（cobolExternal.dll.config）

在 **cobolExternal.exe 所在目錄** 會有一個 `cobolExternal.dll.config`（或發佈時產生的設定檔），內容為 XML，例如：

```xml
<?xml version="1.0" encoding="utf-8" ?>
<configuration>
  <appSettings>
    <add key="SourceFolder" value="C:\MyCobol\FD" />
    <add key="OutputFile" value="C:\MyCobol\FD\output\output.csv" />
    <add key="FileExtension" value="*.*" />
    <add key="ComplexityLowThreshold" value="19" />
    <add key="ComplexityAverageThreshold" value="50" />
  </appSettings>
</configuration>
```

| 鍵 | 說明 |
|----|------|
| **SourceFolder** | 來源檔所在目錄（放 .lst / .mst 等檔案的資料夾） |
| **OutputFile** | 產出 CSV 的**完整路徑**（必填）；目錄不存在時會自動建立 |  
| **FileExtension** | 要掃描的副檔名，例如 `*.*` 或 `*.lst`（預設 `*.lst`） |
| **ComplexityLowThreshold** | Low 複雜度上限（選填，預設 `19`）。DET ≤ 此值為 Low。 |
| **ComplexityAverageThreshold** | Average 複雜度上限（選填，預設 `50`）。20 ≤ DET ≤ 此值為 Average，DET > 此值為 High。 |

---

### 步驟 2：把來源檔放到 SourceFolder

將要分析的 COBOL 列表檔（.lst、.mst 等）複製到你設定的 **SourceFolder**。

---

### 步驟 3：執行程式

1. 開啟「命令提示字元」或 PowerShell
2. 切到程式所在目錄，例如：
   ```batch
   cd C:\Development\HISCore\functionpoint\CobolFD\publish
   ```
3. 執行：
   - **正式掃描並產出 CSV**（依 config 的 SourceFolder / OutputFile）：
     ```batch
     cobolExternal.exe
     ```
   - **僅測試 DET 計算**（使用 publish 內建的 `layout` 樣本）：
     ```batch
     cobolExternal.exe --test
     ```
   - **驗證單一檔案的 DET**：
     ```batch
     cobolExternal.exe --verify "C:\MyCobol\FD\MYFILE.mst"
     ```

---

## 四、執行後會發生什麼？

1. 程式會掃描 **SourceFolder** 下符合 **FileExtension** 的檔案（排除輸出檔與 .log）
2. 擷取每個檔案的 FD 區塊，計算 Size、DET、複雜度
3. 將結果寫入 **OutputFile**（CSV）
4. 在輸出目錄產生 **Log**：`cobolfd_yyyyMMdd_HHmmss.log`（每檔 Processed/Error、摘要）
5. 若寫入主要輸出檔失敗（例如檔案被佔用），會自動改寫到備援檔 `output_yyyyMMdd_HHmmss.csv`

---

## 五、CSV 與複雜度說明

- **CSV 欄位順序**：名稱（FD 名稱）、檔名、日期、大小、DET、複雜度
- **複雜度閾值**（依 DET，可於 App.config 調整）：
  - **Low**：DET ≤ 19（預設，包含 DET=0）
  - **Average**：20 ≤ DET ≤ 50（預設）
  - **High**：DET > 50（預設）

---

## 六、常見問題（FAQ）

**Q：找不到 SourceFolder？**  
A：請檢查 config 裡的 **SourceFolder** 路徑是否存在，且為絕對路徑。

**Q：OutputFile 一定要設嗎？**  
A：是，**OutputFile 必填**。請在 config 中設定產出 CSV 的完整路徑。

**Q：出現「無法存取檔案」？**  
A：可能是輸出 CSV 被其他程式（如 Excel）開啟。關閉後再執行，或讓程式自動寫入備援檔。

**Q：--test 是做什麼的？**  
A：使用程式目錄下的 **layout** 資料夾內 .mst 樣本，測試 DET/Size/複雜度計算，不寫入你的 SourceFolder。

**Q：--verify 是做什麼的？**  
A：對**單一檔案**做 DET 驗證，在畫面上列出每個 FD 的 DET、Size 與葉節點欄位，方便除錯。

**Q：日誌在哪裡？**  
A：與 **OutputFile** 同目錄，檔名為 `cobolfd_yyyyMMdd_HHmmss.log`。

---

## 七、目錄結構建議（範例）

```
C:\MyCobol\FD\           ← SourceFolder（放 .lst / .mst）
C:\MyCobol\FD\output\    ← 放 output.csv 與 .log
```

只要在 config 裡把 **SourceFolder**、**OutputFile** 指到上述路徑即可；輸出目錄不存在時會自動建立。

---

若仍有問題，請查看輸出目錄下的 **cobolfd_*.log**，內有每檔處理結果與錯誤訊息。

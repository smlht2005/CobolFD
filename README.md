# CobolFD — COBOL FD 區塊 DET/Size 產出工具

掃描 COBOL 來源檔（.mst / .trn 等），擷取 FD 區塊、計算每個 FD 的 **Size（bytes）** 與 **DET（Data Element Types）**，並依複雜度產出 CSV 供功能點分析使用。

## 設定（App.config）

| 鍵 | 說明 |
|----|------|
| **SourceFolder** | 來源檔所在目錄（例：`C:\Development\cobolcode\FD`）。 |
| **OutputFile** | 產出 CSV 的完整路徑（例：`C:\Development\cobolcode\FD\output.csv`）；必填；目錄不存在時會自動建立。 |
| **FileExtension** | 掃描副檔名（預設 `*.*`）。 |

## 輸出

- **CSV 欄位**：檔名、日期、名稱（FD 名稱）、大小、DET、複雜度。
- **Log**：同輸出目錄下產生 `cobolfd_yyyyMMdd_HHmmss.log`（每檔 Processed/Error、摘要）。
- **排除**：不掃描與 OutputFile 同路徑的檔案及 `*.log`。

## 執行方式

```bash
cd CobolFD
dotnet build cobolExternal.csproj
cd bin/Debug/net8.0
./cobolExternal.exe
```

- **`--test`**：以專案內 `layout/` 樣本測試 DET 計算。
- **`--verify <檔名>`**：驗證單一檔案之 DET 產出。

## 技術要點

- **FD 擷取**：依 `FD 名稱` 或 `01 … EXTERNAL` 辨識區塊；行尾正規化（\r\n/\r→\n）避免 Unix 行尾檔整檔當一行導致 DET=0。
- **DET/Size**：由 sharelib（CobolLayoutAnalyzer、ComplexityEvaluator）計算；複雜度閾值：Low ≤5、Average 5–15、High >15。

## 更新摘要

- 行尾正規化：修正 HZ_ATLH.MST 等僅 `\n` 行尾檔 DET=0。
- OutputFile 必填驗證、輸出目錄自動建立。
- 擷取所有 FD（不再僅 EXTERNAL）、逐檔 try/catch、寫檔失敗時改寫備援檔。

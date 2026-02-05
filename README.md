# CobolFD — COBOL FD 區塊 DET/Size 產出工具

掃描 COBOL 來源檔（.mst / .trn 等），擷取 FD 區塊、計算每個 FD 的 **Size（bytes）** 與 **DET（Data Element Types）**，並依複雜度產出 CSV 供功能點分析使用。

## 設定（App.config）

| 鍵 | 說明 |
|----|------|
| **SourceFolder** | 來源檔所在目錄（例：`C:\Development\cobolcode\FD`）。 |
| **OutputFile** | 產出 CSV 的完整路徑（例：`C:\Development\cobolcode\FD\output.csv`）；必填；目錄不存在時會自動建立。 |
| **FileExtension** | 掃描副檔名（預設 `*.*`）。 |
| **ComplexityLowThreshold** | Low 複雜度上限（預設 `19`）。DET ≤ 此值為 Low。 |
| **ComplexityAverageThreshold** | Average 複雜度上限（預設 `50`）。20 ≤ DET ≤ 此值為 Average，DET > 此值為 High。 |

## 輸出

- **CSV 欄位順序**：名稱（FD 名稱）、檔名、日期、大小、DET、複雜度。
- **Log**：同輸出目錄下產生 `cobolfd_yyyyMMdd_HHmmss.log`（每檔 Processed/Error、摘要）。
- **排除**：不掃描與 OutputFile 同路徑的檔案及 `*.log`。

## 建置與執行

### 建置

**方式一：使用方案檔建置（推薦）**
```bash
cd CobolFD
dotnet build CobolFD.sln -c Release
```

**方式二：使用專案檔建置**
```bash
cd CobolFD
dotnet build cobolExternal.csproj -c Release
```

> **注意**：目錄中包含多個專案檔，必須明確指定要建置的檔案（`.sln` 或 `.csproj`），不能只執行 `dotnet build`。

建置輸出位於 `bin/Release/net48/`

### 部署

自動部署（Release 建置後自動執行）：
- 建置完成後會自動執行 `deploy-net48.ps1` 部署到 `publish/net4.8/`

手動部署：
```powershell
.\deploy-net48.ps1 -Configuration Release
```

### 執行方式

**方式一：使用 dotnet run（開發模式）**
```bash
cd CobolFD
dotnet run --project cobolExternal.csproj
```

或帶參數執行：
```bash
dotnet run --project cobolExternal.csproj -- --test
```

**方式二：直接執行編譯後的執行檔（推薦）**
```bash
cd publish/net4.8
.\cobolExternal.exe
```

或從建置輸出執行：
```bash
cd bin/Release/net48
.\cobolExternal.exe
```

**命令參數：**
- **`--test`**：以專案內 `layout/` 樣本測試 DET 計算。
- **`--verify <檔名>`**：驗證單一檔案之 DET 產出。

> **注意**：
> - `dotnet build` 和 `dotnet run` 是不同的命令
> - `dotnet build` 用於建置專案，不接受 `run` 參數
> - `dotnet run` 用於建置並執行專案，需要指定 `--project` 參數

## 技術要點

- **FD 擷取**：依 `FD 名稱` 或 `01 … EXTERNAL` 辨識區塊；行尾正規化（\r\n/\r→\n）避免 Unix 行尾檔整檔當一行導致 DET=0。
- **DET/Size**：由 sharelib（CobolLayoutAnalyzer、ComplexityEvaluator）計算。
- **複雜度閾值**（可於 App.config 設定）：
  - **Low**：DET ≤ 19（預設，包含 DET=0）
  - **Average**：20 ≤ DET ≤ 50（預設）
  - **High**：DET > 50（預設）

## 技術規格

- **目標框架**：.NET Framework 4.8
- **建置工具**：.NET SDK（支援 SDK-style 專案格式）

## 更新摘要

- **2026-02-05**：
  - 遷移至 .NET Framework 4.8，新增自動部署腳本
  - 調整 CSV 輸出順序（名稱改為第一欄）
  - 複雜度閾值改為從 App.config 讀取（可動態調整）
  - 修正 DET=0 時輸出 0 並評估為 Low（不再輸出 "-"）
- 行尾正規化：修正 HZ_ATLH.MST 等僅 `\n` 行尾檔 DET=0。
- OutputFile 必填驗證、輸出目錄自動建立。
- 擷取所有 FD（不再僅 EXTERNAL）、逐檔 try/catch、寫檔失敗時改寫備援檔。

-----

## 🧼 Code Smell Detector (Open-Source)

The **Code Smell Detector** is an open-source command-line tool crafted to help developers enhance the quality, maintainability, and security of their JavaScript codebases. It automatically identifies common "code smells" such as syntax errors, unused code, duplicated sections, and potential security vulnerabilities.

**Current Language Support:** JavaScript.
**Planned Support:** Future versions will expand to include TypeScript, Python, and other popular programming languages.

-----

## ✨ Features

  * **Syntax Error Detection:** Pinpoints and reports invalid JavaScript syntax.
  * **Unused Code Identification:** Flags variables and functions that are declared but never utilized, helping to reduce code bloat.
  * **Duplicate Code Analysis:** Locates and highlights repetitive code blocks, suggesting refactoring opportunities.
  * **Security Vulnerability Spotting:** Identifies common patterns that could lead to security risks (e.g., `eval()` usage).
  * **Easy Integration:** Designed for seamless incorporation into existing development workflows and CI/CD pipelines.
  * **Open-Source & Extensible:** Built with an open architecture, encouraging community contributions and custom rule development.

-----

## 📦 Installation

To get started with the Code Smell Detector, clone the repository and install its dependencies:

```bash
git clone https://github.com/yogeshd278/code-smell-detector.git
cd code-smell-detector
npm install
```

-----

## 🚀 Usage

Once installed, you can run the Code Smell Detector against your files or directories.

### Scanning a Single File

To analyze a specific JavaScript file:

```bash
npx code-smell-detector src/components/MyComponent.js
```

### Scanning a Directory

To scan all JavaScript files within a particular directory (and its subdirectories):

```bash
npx code-smell-detector --dir src/
```

### Understanding the Output

The tool provides clear, actionable feedback for each detected code smell. Here's an example of what you might see:

```
Scanning: src/utils/dataProcessor.js

[WARNING] Unused Variable: 'tempData' declared but not used.
  Location: src/utils/dataProcessor.js:10:9

[ERROR] Syntax Error: Unexpected token '('. Missing semicolon?
  Location: src/utils/dataProcessor.js:25:20

[SUGGESTION] Duplicate Code Block: Similar code found at lines 50-55.
  Location: src/utils/dataProcessor.js:30:1 - src/utils/dataProcessor.js:34:15

[SECURITY] Potential Vulnerability: Direct use of 'innerHTML' without sanitization.
  Location: src/utils/dataProcessor.js:40:12

Scan Summary: 1 Error, 1 Warning, 1 Suggestion, 1 Security Alert found.
```

-----

## ⚙️ Configuration (Optional)

Customize the detector's behavior by creating a `.codesmelldetectorrc.json` file at the root of your project.

```json
{
  "ignore": [
    "node_modules/**",
    "dist/**",
    "*.test.js"
  ],
  "rules": {
    "no-unused-vars": "error",
    "no-duplicate-code": "warn",
    "no-eval": "off",
    "max-lines-per-function": ["warn", { "max": 50 }]
  }
}
```

### Configuration Options:

  * `ignore` (array of strings): Specify file paths or glob patterns to exclude from the scan.
  * `rules` (object): Control individual code smell rules.
      * Set a rule to `"off"` to disable it.
      * Set to `"warn"` to report it as a warning.
      * Set to `"error"` to report it as an error.
      * Some rules may accept additional options (e.g., `["warn", { "max": 50 }]`).

-----

## 👋 Contributing

We warmly welcome contributions to the Code Smell Detector\! Your input helps us make this tool better for everyone.

  * **Bug Reports:** Encountered a bug? Please open an issue on our [GitHub Issues page](https://www.google.com/search?q=https://github.com/yogeshd278/code-smell-detector/issues) with a detailed description and steps to reproduce.
  * **Feature Requests:** Have an idea for a new feature or improvement? Submit an enhancement request on the [GitHub Issues page](https://www.google.com/search?q=https://github.com/yogeshd278/code-smell-detector/issues).
  * **Code Contributions:**
    1.  Fork the repository.
    2.  Create a new branch for your feature or bug fix (`git checkout -b feature/my-awesome-feature`).
    3.  Make your changes, ensuring code quality and test coverage.
    4.  Commit your changes with a clear message (`git commit -m 'feat: Add new rule for XYZ'`).
    5.  Push your branch to your fork (`git push origin feature/my-awesome-feature`).
    6.  Open a Pull Request to the main repository, explaining your changes.

-----

## 📄 License

This project is open-source and distributed under the **MIT License**. For more details, please refer to the `LICENSE` file in the repository.

-----

## ❓ Support & Questions

If you have any questions, need assistance, or just want to discuss the project, please use the [GitHub Issues page](https://www.google.com/search?q=https://github.com/yogeshd278/code-smell-detector/issues). We're here to help\!

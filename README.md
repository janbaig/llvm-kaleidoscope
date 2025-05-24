# LLVM-Kaleidoscope

Designing a compiler leveraging LLVM for code generation, inspired by the architecture of the official LLVM Kaleidoscope tutorial

```bash
// Compiling on macOS
clang++ -g -I/opt/homebrew/opt/llvm/include -L/opt/homebrew/opt/llvm/lib -lLLVM -o main main.cpp
```
```bash
// Or run
clang++ -g -O3 main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -o main
```
# Kristopher38's OpenComputers debugger

Have you ever wanted to look at the values of variables in your OC scipt, but had to resort to `print`s scattered around your code? Or maybe you wanted to trace its execution to see what's causing that weird behavior that you don't understand? Search no more! KDB is going to fullfill all your needs, including, but not limited to:
- breakpoints
- step-by-step execution
- variable watches

### But I thought Lua's debug library was disabled in OC?
You're correct, some functions are indeed not available and KDB does not rely on them. Clever tricks are used instead, which involve parsing the source code and injecting the debugger's code into it to be able to provide debugging-like functionality.

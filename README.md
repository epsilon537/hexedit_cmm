HexEdit Binary File Editor for Color Maximite 2 by Epsilon
----------------------------------------------------------
Current Version: 0.2

ChangeLog
---------
0.2:
- Documentation updates.
- Fixed error 'Function name + variable name must be less than 33 characters'.
- Using "." instead of " " in ASCII block locations past file size.
- Using "?" instead of non-printable characters in ASCII block.
- Fixed program crash when insert is pressed.
- Added Loading..., Inserting..., Deleting..., Filling... indications.
- Fixed heap memory issues.
- Incread File Size Limit to 1.6MB

0.1:
- Initial version.

Description
-----------
HexEdit allows you to edit binary files, i.e. files that you can't edit with a regular text editor such as the built-in editor.

The file's contents are shown as hexadecimal byte values and as ASCII code in two side-by-side blocks. Changes can be made both in the hex table section and in the ASCII section.

The usual file navigation with cursors keys, page up&down, home, end, etc. should work as expected.

Hexedit is a full-screen console-only editor. I does not work over serial, sorry.

Press F1 to get help on key bindings.

Current limitations
-------------------
File Size Limit: 1.6MB
Byte mode only. No support (yet?) for 16-bit and 32-bit modes.
Console only.


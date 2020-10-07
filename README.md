HexEdit Binary File Editor for Color Maximite 2 by Epsilon
----------------------------------------------------------
Current Version: 0.6

ChangeLog
---------
0.6:
- Fixed file modified indication not always reported in footer.

0.5:
- Fixed End Key not entirely behaving as expected.
- Show error message when an invalid goto address is entered.

0.4:
- New feature: Ctrl-S find byte sequence.
- Added column header, contributed by Michael "twofingers".
- Modified some key bindings (Sorry!). Load and Save moved to F3 and F2. Ctrl-S is now bound to the search function.
- Increased file size limit to 3MB.

0.3:
- Fixed cosmetic bug where first character sometimes remains inverted right after loading a file.
- Fixed bug with goto when in ASCI column.
- Fixed ctrlF fill.
- New feature: Ctrl-T toggles between different word sizes: 8-bit, 16-bit, 32-bit and 64-bit.
- New feature: Ctrl-E exports part of file as text or as binary.
- Reduced redraw artifacts when editing.
- Added file size to footer.

0.2:
- Documentation updates.
- Fixed error 'Function name + variable name must be less than 33 characters'.
- Using "." instead of " " in ASCII block locations past file size.
- Using "?" instead of non-printable characters in ASCII block.
- Fixed program crash when insert is pressed.
- Added Loading..., Inserting..., Deleting..., Filling... indications.
- Fixed heap memory issues.
- Increase File Size Limit to 1.6MB

0.1:
- Initial version.

Description
-----------
HexEdit allows you to edit binary files, i.e. files that you can't edit with a regular text editor such as the built-in editor.

The file's contents are shown as hexadecimal byte values and as ASCII code in two side-by-side blocks. Changes can be made both in the hex table section and in the ASCII section.

The usual file navigation with cursors keys, page up&down, home, end, etc. should work as expected.

Hexedit is a full-screen console-only editor. I does not work over serial, sorry.

The file to edit can be passed in as a command line argument.

Press F1 to get help on key bindings.

Current limitations
-------------------
File Size Limit: 3MB
Console only.


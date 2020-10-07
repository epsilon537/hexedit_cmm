OPTION EXPLICIT
OPTION DEFAULT NONE
OPTION BASE 0
OPTION CONSOLE SCREEN

CONST VERSION$ = "0.6"

CONST NUM_BYTES_PER_ROW% = 16
CONST BUF_SIZE% = 3*1024*1024

CONST NUM_ROWS% = 45
CONST MAX_TOP_LEFT_FILE_OFFSET% = ((BUF_SIZE%\NUM_BYTES_PER_ROW%)-NUM_ROWS%+1)*NUM_BYTES_PER_ROW%
CONST START_ROW% = 2
CONST START_COL% = 13
CONST START_COL_ASC% = (START_COL% + NUM_BYTES_PER_ROW%*3 + 1)
CONST NUM_ADDR_DIGITS% = 8
CONST CURSOR_BLINK_PERIOD% = 500
CONST NON_PRINTABLE_CHAR_INDICATOR$ = "?" 'For non-printable characters this character is shown 
                                          'instead.

'--> The table below is used to convert a byte's column value to a screen X position.
'Bytes are clumped together or space out depending on selected word size. E.g. 12 34, or 1234, etc.
'The rows in the table correspond to the 4 different word sizes, the columns to column values.
CONST NUM_WORD_SIZES% = 4
DIM COL_TO_X%(NUM_BYTES_PER_ROW%-1, NUM_WORD_SIZES% - 1)
COL_TO_X_DATA:
  DATA 0, 1*3, 2*3, 3*3, 4*3, 5*3, 6*3, 7*3, 8*3, 9*3, 10*3, 11*3, 12*3, 13*3, 14*3, 15*3
  DATA 0,   2, 2*3,   8, 4*3,  14, 6*3,  20, 8*3,  26, 10*3,   32, 12*3,   38, 14*3,   44
  DATA 0,   2,   4,   5, 4*3,  14,  16,  18, 8*3,  26,   28,   30, 12*3,   38,   40,   42
  DATA 0,   2,   4,   6,   8,  10,  12,  14, 8*3,  26,   28,   30,   32,   34,   36,   38

SUB initColToX 'This is a workaround for 2D array pre-initialization.
  LOCAL ii%,jj%

  RESTORE COL_TO_X_DATA
  FOR jj%=0 TO NUM_WORD_SIZES%-1
    FOR ii%=0 TO NUM_BYTES_PER_ROW%-1
      READ COL_TO_X%(ii%, jj%) 
    NEXT ii%
  NEXT jj%
END SUB

initColToX
'<--

DIM filename$ = ""
DIM fileSize% = 0
DIM fileIsModified% = 0
DIM exitRequested% = 0

'Offset in file corresponding to element on top left of the screen.
DIM topLeftFileOffset% = 0

'Maintain a parallel bit array indicating if given position has been modified
'This array is to be considered private, only to be accessed via the setModified/isModified 
'accessors.
DIM modified%(BUF_SIZE%/64)

'Memory buffer holding the file's contents.
'This array is to be considered private, only to be accessed via the readByteBuf/writeByteBuf 
'accessors.
DIM byteBuf%(BUF_SIZE/8)
DIM byteBufAddr% = PEEK(VARADDR byteBuf%())

'--> These variables are inputs to positionCursorInTable/ASCblock.
DIM crsrRow% = 0, crsrCol% = 0, crsrNibbleOffset% = 0
'<--

'--> The following variables can only be modified by positionCursorInTable/ASCblock:
DIM crsrScrnXpos% = 0, crsrScrnYpos% = 0
DIM crsrFileOffset% 'File offset corresponding to element at cursor
'These are booleans. If the cursor is in the ASCII block, both are 0.
DIM crsrOnLeftNibble%, crsrOnRightNibble%
'<--

'A semaphore from pageRefresh to positionCursorInTable/ASCblock.
DIM pageRefreshed%=0

'Word size (in bytes) in hex table: 1, 2, 4 or 8
'Note that this editor fundamentally remains a byte level editor. Word size just affect how the
'data is formatted in the hex table.
'Also note that because this is an editor for CMM2, little endianness is assumed.
DIM wordSize% = 1
DIM log2WordSize% = 0 '0-3 corrspondening to word sizes 1, 2, 4 and 8. 

MODE 1, 8
FONT 1, 1

PAGE WRITE 1
COLOUR RGB(WHITE), RGB(BLUE)
CLS
PAGE WRITE 0
COLOUR RGB(WHITE), RGB(BLUE)
CLS

printHeader
LINE 0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-6,MM.HRES-1,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-6,,RGB(WHITE)
promptMsg "Starting...", 1
printFooter

refreshPage

IF MM.CMDLINE$ <> "" THEN
  checkAndLoad MM.CMDLINE$ 'File to edit can be passed in on command line.
ELSE
  checkAndLoad promptForText$("Load File: ")
ENDIF

DIM blinkCursorFlag% = 0
settick CURSOR_BLINK_PERIOD%, blinkCursorInt, 1

'--> The main loop:
DO WHILE exitRequested% = 0
  IF blinkCursorFlag% THEN
    blinkCursor
    blinkCursorFlag% = 0
  ENDIF

  checkKey
  printFooter 'Reprint footer because it contains status info such as file is (un)modified.
LOOP
'<--

CLS RGB(BLACK)

EndOfProg:
CLEAR
END

'--> Memory buffer accessor functions:
'Write byte specified by val$ in memory buffer at given offset.
SUB writeByteBuf(offset%, val$)
  POKE BYTE byteBufAddr%+offset%, ASC(val$)
END SUB

'Read byte from memory buffer at given offset.
FUNCTION readByteBuf$(offset%)
  readByteBuf$ = CHR$(PEEK (BYTE byteBufAddr% + offset%))
END FUNCTION
'<--

'--> modified bit array accessor functions:
'Mark file position specified by offset as modified (isMod=1) or not modified (isMod=0)
SUB setModified(offset%, isMod%)
  LOCAL idx% = offset%>>6
  LOCAL bitpos% = 1<<(offset% AND 63)

  IF isMod% THEN
    modified%(idx%) = modified%(idx%) OR bitpos%
    fileIsModified% = 1 'Indicate file as modified.
  ELSE
    modified%(idx%) = modified%(idx%) AND (-1 XOR bitpos%)
  ENDIF  
END SUB

'Returns true if file position specified by offset is marked as modified.
FUNCTION isModified%(offset%)
  LOCAL idx% = offset%>>6
  LOCAL bitpos% = 1<<(offset% AND 63)
  isModified% = (modified%(idx%) AND bitpos%) <> 0
END FUNCTION
'<--

'Returns true if the cursor is in the table, not the ASCII block
FUNCTION cursorIsInTable%()
  cursorIsInTable% = (crsrOnLeftNibble%=1) OR (crsrOnRightNibble%=1)
END FUNCTION

SUB saveFile
  LOCAL location% = 0
  OPEN filename$ FOR RANDOM AS #1 'Random access because we only save what's been modified.

  DO WHILE location% < fileSize%
    IF isModified%(location%) THEN
      SEEK #1, location%+1 'loc starts at 0, file offset starts at 1.
      PRINT #1, readByteBuf$(location%);
      setModified location%, 0
    ENDIF

    location% = location% + 1
  LOOP

  CLOSE #1
  fileIsModified% = 0
END SUB

SUB loadFile
  LOCAL location% = 0

  promptMsg "Loading...", 1
  OPEN filename$ FOR INPUT AS #1

  DO WHILE location% < fileSize%
    writeByteBuf location%, INPUT$(1, #1)
    setModified location%, 0
    location% = location% + 1
  LOOP

  CLOSE #1

  promptMsg "", 0
END SUB

'Only exit this function if we have established a valid non-empty filename.
SUB checkAndLoad(fileToLoad$)
  LOCAL dummy$
  LOCAL fileToLoadl$ = fileToLoad$
  LOCAL fileIsValid% = 0
  LOCAL fileSizel%

  DO WHILE fileIsValid% = 0
    DO WHILE fileToLoadl$ = ""
      fileToLoadl$ = promptForText$("Please enter a valid filename: ")
    LOOP

    fileSizel% = MM.INFO(FILESIZE fileToLoadl$)  

    IF fileSizel% = -1 THEN
      IF UCASE$(promptForAnyKey$("File not found. Create File? (Y/N)")) <> "Y" THEN
        fileToLoadl$ = ""
        CONTINUE DO
      ENDIF
      'A new, empty file.
      fileSizel% = 0
      fileIsValid% = 1
    ELSE IF fileSizel% >= BUF_SIZE% THEN
      dummy$ = promptForAnyKey$("File size is larger than supported limit of " + STR$(BUF_SIZE%) + " bytes. Press any key to continue.")
      fileToLoadl$ = ""
      CONTINUE DO
    ELSE
      fileIsValid% = 1
    ENDIF
  LOOP

  filename$ = fileToLoadl$
  fileSize% = fileSizel%

  IF fileSize% > 0 THEN
    loadFile
  ENDIF

  topLeftFileOffset% = 0
  refreshPage
  crsrRow% = 0 : crsrCol% = 0 : crsrNibbleOffset% = 0: positionCursorInTable
END SUB

'Not to be confused with refresRow below. This function is used by exportTxt and prints one line 
'to file #1.
SUB printRow(offset%)
  LOCAL offsetl% = offset%
  LOCAL col%, x%, prevX% = START_COL%
  LOCAL elem$
  LOCAL byteOffset%, wordOffset%

  'File offset in Hex
  PRINT #1, "&H" HEX$(offsetl%,NUM_ADDR_DIGITS%) ":  ";

  'The Hex byte section.
  FOR col% = 0 TO (NUM_BYTES_PER_ROW%-1)
    x% = (START_COL% + COL_TO_X%(col%, log2WordSize%))

    'Offset monotonically increments across the row. This means that for word sizes other than 1,
    'offset no longer directly corresponds to a file offset. Endianness needs to be taken into 
    'account. E.g. for word size 4, the file offsets across the row go like this:
    '03020100 07060504 ...
    'The following two statements make the conversion:
    'Offset of the word we're on.
    wordOffset% = (offsetl%>>log2WordSize%)<<log2WordSize% 
    'From there we find the offset of the byte we're on.
    byteOffset% = wordSize% - 1 - offsetl% + 2*wordOffset% 

    IF byteOffset% < fileSize% THEN
      elem$ = HEX$(ASC(readByteBuf$(byteOffset%)),2)
    ELSE
      elem$ = "--"
    ENDIF

    offsetl% = offsetl% + 1

    PRINT #1, SPACE$(x%-prevX%-2) elem$;
    prevX% = x%
  NEXT col%

  offsetl% = offset%
  col%=0

  PRINT #1, SPACE$(START_COL_ASC%-prevX%-2);

  'The ASCII block section.
  FOR col% = 0 TO (NUM_BYTES_PER_ROW%-1)
    IF offsetl% < fileSize% THEN
      elem$ = readByteBuf$(offsetl%)
      IF NOT isPrintable%(elem$) THEN
        elem$ = NON_PRINTABLE_CHAR_INDICATOR$
      ENDIF
    ELSE
      elem$ = "."
    ENDIF

    offsetl% = offsetl%+1

    PRINT #1, elem$;
  NEXT col%

  PRINT #1, "" 'newline
END SUB

'Export from startAddr to endAddr as text to exportFilename.
'We actually start from the row holding startAddr so we maintain
'the same row/address alignment as shown in the editor.
SUB exportTxt(startAddr%, endAddr%, exportFilename$)
  LOCAL location%
  LOCAL addr% = rowStartAddr%(startAddr%)

  OPEN exportFilename$ FOR OUTPUT AS #1

  DO WHILE addr% <= endAddr%
    printRow addr%
    addr% = addr% + NUM_BYTES_PER_ROW%
  LOOP

  CLOSE #1
END SUB

'Export from startAddr to endAddr as binary to exportFilename.
SUB exportBin(startAddr%, endAddr%, exportFilename$)
  LOCAL location%

  OPEN exportFilename$ FOR OUTPUT AS #1

  FOR location%=startAddr% TO endAddr%
    PRINT #1, readByteBuf$(location%);
  NEXT location%

  CLOSE #1
END SUB

'Refresh the whole page on the screen
SUB refreshPage
  LOCAL row%
  LOCAL offset% = topLeftFileOffset%

  FOR row%=0 TO NUM_ROWS%-1
    refreshRow offset%, row%, 1
  NEXT row%

  BLIT 0, START_ROW%*MM.INFO(FONTHEIGHT), 0, START_ROW%*MM.INFO(FONTHEIGHT), MM.HRES, NUM_ROWS%*MM.INFO(FONTHEIGHT), 1

  'This is a semaphore to signal to positionCursorInTable/ASCblock that the page has been refreshed.
  pageRefreshed% = 1
END SUB

'Refreshes given row number on the screen with the contents at file offset.
'End offset is passed back up to the caller.
SUB refreshRow(offset%, row%, skipBlit%)
  LOCAL offsetl% = offset%
  LOCAL row_l% = row% + START_ROW%
  LOCAL col%, x%, y%
  LOCAL elem$
  LOCAL invert%
  LOCAL byteOffset%, wordOffset%

  y% = row_l%*MM.INFO(FONTHEIGHT)
  x% = 0

  PAGE WRITE 1

  'File offset in Hex
  PRINT @(x%,y%,0) "&H" HEX$(offsetl%,NUM_ADDR_DIGITS%) ":";

  'The Hex byte section.
  FOR col% = 0 TO (NUM_BYTES_PER_ROW%-1)
    invert% = 0
    x% = (START_COL% + COL_TO_X%(col%, log2WordSize%))*MM.INFO(FONTWIDTH)

    'Offset monotonically increments across the row. This means that for word sizes other than 1,
    'offset no longer directly corresponds to a file offset. Endianness needs to be taken into 
    'account. E.g. for word size 4, the file offsets across the row go like this:
    '03020100 07060504 ...
    'The following two statements make the conversion:
    'Offset of the word we're on
    wordOffset% = (offsetl%>>log2WordSize%)<<log2WordSize%
    'From there find the offset of the byte we're on. 
    byteOffset% = wordSize% - 1 - offsetl% + 2*wordOffset% 

    IF byteOffset% < fileSize% THEN
      elem$ = HEX$(ASC(readByteBuf$(byteOffset%)),2)
      IF isModified%(byteOffset%) THEN 'Modified content is shown inverted in the hex table.
        invert% = 2
      ENDIF
    ELSE
      elem$ = "--"
    ENDIF

    offsetl% = offsetl%+1

    PRINT @(x%,y%,invert%) elem$;: PRINT "        ";
  NEXT col%

  offsetl% = offset%
  col%=0
  
  'The ASCII block section.
  FOR col% = 0 TO (NUM_BYTES_PER_ROW%-1)
    x% = (START_COL_ASC% + col%)*MM.INFO(FONTWIDTH)

    IF offsetl% < fileSize% THEN
      elem$ = readByteBuf$(offsetl%)
      IF NOT isPrintable%(elem$) THEN
        elem$ = NON_PRINTABLE_CHAR_INDICATOR$
      ENDIF
    ELSE
      elem$ = "."
    ENDIF

    offsetl% = offsetl%+1

    PRINT @(x%,y%,0) elem$;
  NEXT col%

  PAGE WRITE 0
  IF NOT skipBlit% THEN
    BLIT 0, y%, 0, y%, MM.HRES, MM.INFO(FONTHEIGHT), 1
  ENDIF

  'Return end offset to caller
  offset% = offsetl%
END SUB

SUB printHeader
 LOCAL header$ = "HexEdit V"+VERSION$+" by Epsilon.";
 LOCAL byteNr$ = "             00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F  0123456789ABCDEF"

 'Print inverted
 PRINT @(0,0,2) header$ + SPACE$(MM.HRES/MM.INFO(FONTWIDTH) - LEN(header$))
 'Print CYAN)
 COLOR RGB(CYAN)
 PRINT @(0,MM.INFO(FONTHEIGHT),2) byteNr$ + SPACE$(MM.HRES/MM.INFO(FONTWIDTH) - LEN(byteNr$))
 COLOR RGB(WHITE)
END SUB

SUB printFooter
  LOCAL modifiedIndicator$
  LOCAL filenamel$

  IF fileIsModified% THEN
    modifiedIndicator$ = " (Modified) "
  ELSE
    modifiedIndicator$ = " (Unmodified) "
  ENDIF

  IF filename$ = "" THEN
    filenamel$ = "(...)" 'Just until we have a filename.
  ELSE
    filenamel$ = filename$
  ENDIF

  LOCAL footerLeft$ = "File: " + filenamel$ + modifiedIndicator$ + " Size: " + STR$(fileSize%) + " bytes   Mode: " + STR$(wordSize%*8) +"-bit"
  LOCAL footerRight$ = "F1 = Help  "

  'Print inverted.
  PRINT @(0,(NUM_ROWS%+4)*MM.INFO(FONTHEIGHT),2) footerLeft$ + SPACE$(MM.HRES/MM.INFO(FONTWIDTH) - LEN(footerLeft$) - LEN(footerRight$)) + footerRight$;
END SUB

'Prints the given text on the prompt line, then waits for input. 
'The input string is returned to the caller.
FUNCTION promptForText$(text$)
  LOCAL inputStr$
  PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) text$;
  INPUT "", inputStr$
  emptyInputBuffer
  PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) SPACE$(MM.HRES/MM.INFO(FONTWIDTH));
  promptForText$ = inputStr$  
END FUNCTION

'Prints the given text on the prompt line, then waits for the user to press any key. 
'The pressed key is returned to the caller.
FUNCTION promptForAnyKey$(text$)
  LOCAL pressedKey$
  LOCAL latchedTime% = INT(TIMER)

  PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) text$;
  LOCAL crsrPos% = (LEN(text$)+1)*MM.INFO(FONTWIDTH)
  LOCAL invert% = 0

  emptyInputBuffer

  'An overly complex way of getting a blinking cursor at the prompt...
  DO: 
    pressedKey$ = INKEY$ 
    PRINT @(crsrPos%, (NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4, invert%) " ";
    IF (INT(TIMER) > latchedTime% + CURSOR_BLINK_PERIOD%) THEN
      invert% = invert% XOR 2
      latchedTime% = INT(TIMER)
    ENDIF
  LOOP UNTIL pressedKey$ <> ""

  PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) SPACE$(MM.HRES/MM.INFO(FONTWIDTH));

  promptForAnyKey$ = pressedKey$
END FUNCTION

'If on%=1, prompt text is shown. If on%=0 prompt text is removed.
SUB promptMsg(text$, on%)
  IF on%=1 THEN
    PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) text$;
    emptyInputBuffer
  ELSE
    PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) SPACE$(MM.HRES/MM.INFO(FONTWIDTH));
  ENDIF
END SUB

'Return true if given character is printable
FUNCTION isPrintable%(char$)
  isPrintable% = (char$ >= CHR$(32))
END FUNCTION

'Returns true if the given address (file offset) is currently shown on the screen. 
'If includeLastRow% = 0, pretend that the last row is not shown on the screen.
FUNCTION addrIsOnScreen%(addr%, includeLastRow%)
  addrIsOnScreen% = (addr% >= topLeftFileOffset%) AND (addr% <= topLeftFileOffset% + NUM_BYTES_PER_ROW%*(NUM_ROWS%-1+includeLastRow%) - 1)
END FUNCTION

'Return the address/offset of the 1st byte in the row containing given address.
FUNCTION rowStartAddr%(addr%)
  rowStartAddr% = (addr%\NUM_BYTES_PER_ROW%)*NUM_BYTES_PER_ROW%
END FUNCTION

'Convert hex table column position to file offset (rel. to start of column) taking word size into 
'account.
FUNCTION tblColToFileOffset%(col%)
  tblColToFileOffset% = (col%\wordSize% + 1)*wordSize% - 1 - (col% MOD wordSize%)
END FUNCTION

'Convert file offset to hex table column position taking word size into account
FUNCTION fileOffsetToTblCol%(fOffset%)
  LOCAL byteOffsetInRow% = fOffset% - rowStartAddr%(fOffset%)
  LOCAL wordOffsetInRow% = (byteOffsetInRow%\wordSize%)*wordSize%
  fileOffsetToTblCol% = 2*wordOffsetInRow% + wordSize% - 1 - byteOffsetInRow%
END FUNCTION

'Convert file offset to ASCII block column
FUNCTION fileOffsetToASCcol%(offset%)
  fileOffsetToASCcol% = (offset% - topLeftFileOffset%) MOD NUM_BYTES_PER_ROW%
END FUNCTION

'Position the cursor at the given address (file offset). Scroll if necessary.
'Set allowCursorOnLastRow%=1 if you want to allow positioning of cursor on last row rather than scrolling.
SUB positionCursorAtAddr(addr%, allowCursorOnLastRow%)
  LOCAL addrl% = addr%

  IF addrl% >= BUF_SIZE% THEN
    addrl% = BUF_SIZE%-1
  ENDIF

  IF addrIsOnScreen%(addrl%, allowCursorOnLastRow%) = 0 THEN
    IF addrl% > topLeftFileOffset% THEN 'Move forward to middle of screen.
      topLeftFileOffset% = (addrl%\NUM_BYTES_PER_ROW% - NUM_ROWS%\2)*NUM_BYTES_PER_ROW% 
    ELSE 'Move backward to top of screen.
      topLeftFileOffset% = rowStartAddr%(addrl%)
    ENDIF
    refreshPage
  ENDIF

  crsrRow% = (addrl% - topLeftFileOffset%)\NUM_BYTES_PER_ROW%
  crsrNibbleOffset% = 0

  IF cursorIsInTable%() THEN
    crsrCol% = fileOffsetToTblCol%(addrl%)
    positionCursorInTable
  ELSE
    crsrCol% = fileOffsetToASCcol%(addrl%)
    positionCursorInASCblock
  ENDIF
END SUB

'Position cursor in the hex table at crsrRow/crsrCol/crsrNibbleOffset
'crsrRow/Col are byte element positions, not screen coordinates. crsrNibbleOffset is 0 or 1 for 
'left or right nibble.
'crsrRow increments from left to right. crsCol increments from top to bottom.
'This sub won't scroll the page. We're positioning the cursor somewhere on the current page.
SUB positionCursorInTable
  'Un-Reverse previous char position, unless we just had a page refresh.
  IF NOT pageRefreshed% THEN
    drawCharAtCursor 0
  ELSE
    pageRefreshed% = 0
  ENDIF

  'Start column + word offset including spacing between words + byte offset in word + nibble offset 
  'in bytes.
  crsrScrnXpos% = START_COL% + COL_TO_X%(crsrCol%, log2WordSize%) + crsrNibbleOffset%
  'crsrScrnXpos% = START_COL% + (crsrCol%\wordSize%)*(wordSize%*3) + (crsrCol% MOD wordSize%)*2 + crsrNibbleOffset%
  crsrScrnYpos% = crsrRow% + START_ROW%
  crsrOnLeftNibble% = (crsrNibbleOffset%=0)
  crsrOnRightNibble% = (crsrNibbleOffset%=1)
  crsrFileOffset% = topLeftFileOffset% + crsrRow%*NUM_BYTES_PER_ROW% + tblColToFileOffset%(crsrCol%)

  'Don't exceed buffer size
  IF crsrFileOffset% >= BUF_SIZE% THEN
    positionCursorAtAddr(BUF_SIZE%-1), 1
  ENDIF

  drawCharAtCursor 2 '2 indicates print reversed.
END SUB

'Position cursor in the ASCII block at crsrRow/crsrCol.
'crsrRow/Col are element positions, not screen coordinates.
SUB positionCursorInASCblock
  'Un-Reverse previous char position, unless we just had a page refresh.
  IF NOT pageRefreshed% THEN
    drawCharAtCursor 0
  ELSE
    pageRefreshed% = 0
  ENDIF

  crsrScrnXpos% = START_COL_ASC% + crsrCol%
  crsrScrnYpos% = crsrRow% + START_ROW%
  crsrOnLeftNibble% = 0
  crsrOnRightNibble% = 0
  crsrFileOffset% = topLeftFileOffset% + crsrRow%*NUM_BYTES_PER_ROW% + crsrCol%

  'Don't exceed buffer size
  IF crsrFileOffset% >= BUF_SIZE% THEN
    positionCursorAtAddr(BUF_SIZE%-1), 1
  ENDIF

  drawCharAtCursor 2 '2 indicates print reversed.
END SUB

'The IRQ just sets the flag, which'll cause blinkCursor below to be called from the main loop.
SUB blinkCursorInt
  blinkCursorFlag% = 1
END SUB

SUB blinkCursor
  STATIC invert%=2
  'A cursor is emulated by alternating between regular and reverse print of the character at the 
  'cursor position.
  drawCharAtCursor invert% 
  invert% = invert% XOR 2
END SUB

'Print the character at the cursor position. The actual inputs to this function are globals:
'crsrFileOffset (indicating where in the file the character to be printed is located)
'crsrScreenXpos/crsrScrnYpos and crsrOnLeft/RightNibble.
'The invert flag indicates if the character should be printed in reversed-font.
SUB drawCharAtCursor(invert%)
  LOCAL char$
  LOCAL invertl% = invert%

  IF crsrFileOffset% < fileSize% THEN
    char$ = readByteBuf$(crsrFileOffset%)
    IF crsrOnLeftNibble% = 1 THEN
      char$ = HEX$(ASC(char$)\16, 1)
    ELSEIF crsrOnRightNibble% = 1 THEN
      char$ = HEX$(ASC(char$) AND 15, 1)
    ENDIF
    'If the character position is modified, inverse the inverse flag.
    IF cursorIsInTable%() AND isModified%(crsrFileOffset%) THEN
      invertl% = invertl% XOR 2
    ENDIF
  ELSE 'Cursor is outside of the file boundaries.  
    IF cursorIsInTable%() THEN
      char$ = "-"
    ELSE
      char$ = "."
    ENDIF
  ENDIF

  PRINT @(crsrScrnXpos%*MM.INFO(FONTWIDTH), crsrScrnYpos%*MM.INFO(FONTHEIGHT), invertl%) char$;
END SUB

'help popup is prepared on a separate page in a Box, then shown on page 0 using a sprite.
SUB showHelpPopup
  LOCAL longestStringLen% = LEN("Home 1x = Go To Top of Page")
  LOCAL numLines% = 20
  LOCAL boxWidth% = (longestStringLen%+4)*MM.INFO(FONTWIDTH)
  LOCAL boxHeight% = (numLines%+4)*MM.INFO(FONTHEIGHT)

  PAGE WRITE 2
  BOX 0, 0, boxWidth%, boxHeight%, 4, RGB(RED), RGB(WHITE)
  
  LOCAL x% = 2*MM.INFO(FONTWIDTH)
  LOCAL y% = 2*MM.INFO(FONTHEIGHT)

  PRINT @(x%,y%,2) "    Help - Key Bindings";
  y% = y% + 2*MM.INFO(FONTHEIGHT)
                    
  PRINT @(x%,y%,2) "F1 = Help";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "F2 = Save";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "F3 = Load";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Ctrl-Q = Quit";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Ctrl-E = Export";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Ctrl-F = Fill";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Ctrl-S = Search";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Ctrl-G = Goto";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Ctrl-P = Screenshot";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Ctrl-T = Toggle Word Size";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "INS = Insert one byte";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "DEL = Delete one byte";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Home 1x = Go To Top of Page";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "Home 2x = Go To Top of File";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "End 1x = Go To End of Page";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "End 2x = Go To End of File";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "PgUp = Move one Page Up";
  y% = y% + MM.INFO(FONTHEIGHT)
  PRINT @(x%,y%,2) "PgDn = Move one Page Down";

  PAGE WRITE 0

  SPRITE READ 1, 0 , 0, boxWidth%, boxHeight%, 2
  SPRITE SHOW 1, MM.HRES/2 - boxWidth%/2, MM.VRES/2 - boxHeight%/2, 1
END SUB

SUB removeHelpPopup
  SPRITE CLOSE 1
END SUB

'Scroll down one line by moving the file offset corresponding to the top left of the screen,
'then calling a full page refresh.
SUB scrollLineDown
  IF topLeftFileOffset% >= NUM_BYTES_PER_ROW% THEN
    topLeftFileOffset% = topLeftFileOffset% - NUM_BYTES_PER_ROW%
    refreshPage
  ENDIF
END SUB

'Scroll up one line by moving the file offset corresponding to the top left of the screen,
'then calling a full page refresh,
SUB scrollLineUp
  topLeftFileOffset% = MIN(topLeftFileOffset% + NUM_BYTES_PER_ROW%, MAX_TOP_LEFT_FILE_OFFSET%)
  refreshPage
END SUB

'Scroll one page by moving the file offset corresponding to the top left of the screen,
'then calling a full page refresh.
SUB scrollPageDown
  IF topLeftFileOffset% >= NUM_BYTES_PER_ROW% THEN
    topLeftFileOffset% = MAX(topLeftFileOffset% - NUM_BYTES_PER_ROW%*NUM_ROWS%, 0)
    refreshPage
  ENDIF
END SUB

'Scrollone page by moving the file offset corresponding to the top left of the screen,
'then calling a full page refresh.
SUB scrollPageUp
  topLeftFileOffset% = MIN(topLeftFileOffset% + NUM_BYTES_PER_ROW%*NUM_ROWS%, MAX_TOP_LEFT_FILE_OFFSET%)
  refreshPage
END SUB

'Move the cursor up on the screen, scroll if needed.
SUB cursorUp
  IF crsrRow% > 0 THEN
    crsrRow% = crsrRow% - 1
  ELSE
    scrollLineDown
  ENDIF
  IF cursorIsInTable%() <> 0 THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'Move the cursor down on the screen, scroll if needed.
SUB cursorDown
  IF crsrRow% < NUM_ROWS%-1 THEN
    crsrRow% = crsrRow% + 1
  ELSE
    scrollLineUp
  ENDIF
  IF cursorIsInTable%() <> 0 THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'Move the cursor left on the screen. There are many cases here...
SUB cursorLeft
  LOCAL posInTableNotASC%

  IF cursorIsInTable%() <> 0 THEN
    IF crsrNibbleOffset%=1 THEN 'We're on a right nibble in the hex table. Move to the left nibble.
      crsrNibbleOffset% = 0
      posInTableNotAsc% = 1
    ELSE
      IF crsrCol% > 0 THEN 'We're on the left nibble in the hex table, not in the leftmost column.
        crsrNibbleOffset% = 1 'We move to the right nibble of the previous byte.
        crsrCol% = crsrCol% - 1
        posInTableNotAsc% = 1
      ELSE
        IF crsrRow% > 0 THEN 'We're on the left nibble of the leftmost column, but not on the top row.
          crsrRow% = crsrRow% - 1 'We move one row up, to the last column in the ASCII block.
          crsrCol% = NUM_BYTES_PER_ROW% - 1
          posInTableNotASC% = 0
        ELSE 'crsrRow = 0
          IF topLeftFileOffset% > 0 THEN 'Leftmost column, top row, not the beginning of the file.
            scrollLineDown          'scroll one line, then move to last column in the ASCII block.
            crsrCol% = NUM_BYTES_PER_ROW% - 1
            posInTableNotASC% = 0
          ELSE
            posInTableNotASC% = 1 'Leftmost column, top row, beginning of the file. Do nothing.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ELSE 'In ASC block
    IF crsrCol% > 0 THEN 'We 're in the ASCII block, not first column. Just move left.
      crsrCol% = crsrCol% - 1
      posInTableNotASC% = 0
    ELSE
      posInTableNotASC% = 1 'We're in the ASCII block, first colument. 
      crsrCol% = NUM_BYTES_PER_ROW% - 1 'Jump to last column of hex table on same row
      crsrNibbleOffset% = 1               'Rightmost nibble.
    ENDIF
  ENDIF

  IF posInTableNotASC% THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'stayInBlock indicates whether cursor at the end of the block (table or ASC block)
'should advance to the other block (stayInBlock=0) or go to the next line in the current block 
'(stayInBlock=1). Again, many cases, by similar logic as in cursorLeft above.
SUB cursorRight(stayInBlock%)
  LOCAL posInTableNotASC%

  IF cursorIsInTable%() THEN
    IF crsrNibbleOffset%=0 THEN 'Cursor is on the left digit
      crsrNibbleOffset% = 1
      posInTableNotASC% = 1
    ELSE
      IF crsrCol% < NUM_BYTES_PER_ROW% - 1 THEN 'Cursor is on the right digit but not at end of line
        crsrNibbleOffset% = 0
        crsrCol% = crsrCol% + 1
        posInTableNotASC% = 1
      ELSE IF stayInBlock% = 0 THEN 'At the end of the line, move to the ASCII block
        crsrCol% = 0
        posInTableNotASC% = 0
      ELSE 'At the of the line, Stay in Block, so go to start of next line
        IF crsrRow% < NUM_ROWS% - 1 THEN 'If not at the end of the page, just go down one row
          crsrRow% = crsrRow% + 1
          crsrCol% = 0
          crsrNibbleOffset% = 0
          posInTableNotASC% = 1
        ELSE 'If at the end of the page scroll up one line
          crsrCol% = 0
          crsrNibbleOffset% = 0
          posInTableNotASC% = 1
          scrollLineUp
        ENDIF
      ENDIF
    ENDIF
  ELSE 'In ASC block
    IF crsrCol% < NUM_BYTES_PER_ROW% - 1 THEN 'Not at the end of the line
      crsrCol% = crsrCol% + 1
      posInTableNotASC% = 0
    ELSE 'Last column:
      IF crsrRow% < NUM_ROWS% - 1 THEN 'Not last row:
        crsrRow% = crsrRow% + 1
        crsrCol% = 0
        crsrNibbleOffset% = 0
      ELSE 'Last row:
        scrollLineUp
        crsrCol% = 0
        crsrNibbleOffset% = 0
      ENDIF

      posInTableNotASC% = NOT stayInBlock% 
    ENDIF
  ENDIF

  IF posInTableNotASC% THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'pageUp key handler
SUB pageUp
  scrollPageDown
  IF cursorIsInTable%() <> 0 THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'pageDown key handler
SUB pageDown
  scrollPageUp
  IF cursorIsInTable%() <> 0 THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'The argument indicates how many consecutive times the home key was pressed.
'1x: Put cursor at top of page. 2x: Put cursor at file offset 0.
SUB homeKeyHandler(numConsecPresses%)
  LOCAL addr%
  IF (numConsecPresses% >= 2) THEN
    addr% = 0
  ELSE
    addr% = topLeftFileOffset%
  ENDIF

  positionCursorAtAddr addr%, 1
END SUB

'The argument indicates how many consecutive times the end key was pressed.
'1x: Put cursor at end of page. 2x: Put cursor at EOF.
SUB endKeyHandler(numConsecPresses%)
  LOCAL addr%
  IF (numConsecPresses% >= 2) THEN
    addr% = fileSize% - 1
  ELSE
    addr% = topLeftFileOffset% + NUM_ROWS%*NUM_BYTES_PER_ROW% - 1
  ENDIF

  positionCursorAtAddr addr%, 1
END SUB

'Ctrl-Q (Quit) key handler.
SUB quitKeyHandler
  IF fileIsModified% THEN
    LOCAL yesNo$ = promptForAnyKey$("You have unsaved changes. Are you sure you want to quit? (Y/N)")
    exitRequested% = (UCASE$(yesNo$)="Y")
  ELSE
    exitRequested% = 1
  ENDIF
END SUB

'Ctrl-G (Go To addr) key handler.
SUB gotoKeyHandler
  LOCAL addrStr$ = promptForText$("Go To Address: &H")
  LOCAL addrInt% = -1
  ON ERROR IGNORE 1
    addrInt% = EVAL("&H"+addrStr$)
  IF addrInt% <> -1 THEN
    IF addrInt% >= BUF_SIZE% THEN
      promptMsg "Can't jump beyond max. buffer size of &H" + HEX$(BUF_SIZE) + " bytes.", 1
      EXIT SUB
    ELSE
      promptMsg "Invalid address", 1
    ENDIF
    positionCursorAtAddr addrInt%, 0
  ENDIF
END SUB

SUB loadKeyHandler
  IF fileIsModified% THEN
    IF UCASE$(promptForAnyKey$("You have unsaved changes. Discard changes and open a new file? (Y/N)")) <> "Y" THEN
      EXIT SUB
    ENDIF
    filename$ = ""
    fileSize% = 0
    fileIsModified% = 0
    refreshPage
  ENDIF

  checkAndLoad promptForText$("Load File: ")
END SUB

SUB saveKeyHandler
  promptMsg "Saving...", 1
  saveFile
  refreshPage 'To remove all the location-is-modified indications.
  promptMsg "File saved.", 1
END SUB

SUB toggleKeyHandler
  'Increment with wraparound
  log2wordSize% = (log2wordSize% + 1) AND 3
  wordSize% = 1<<log2wordSize%  'Sync wordSize to log2WordSize
  refreshPage
  positionCursorAtAddr crsrFileOffset%, 1
END SUB

SUB exportKeyHandler
  LOCAL startAddrStr$ = promptForText$("Export from Address: &H")
  LOCAL startAddrInt% = -1
  ON ERROR IGNORE 1
    startAddrInt% = EVAL("&H"+startAddrStr$)

  IF startAddrInt% < 0 THEN
    promptMsg "Start address invalid.", 1
    EXIT SUB
  ENDIF

  LOCAL endAddrStr$ = promptForText$("To Address: &H")
  LOCAL endAddrInt% = -1
  ON ERROR IGNORE 1
    endAddrInt% = EVAL("&H"+endAddrStr$)

  IF endAddrInt% < startAddrInt% THEN
    promptMsg "End address invalid.", 1
    EXIT SUB
  ELSE IF endAddrInt% >= fileSize% THEN
    promptMsg "End address is past file size., 1
    EXIT SUB
  ENDIF

  LOCAL binOrTxt$ = ""
  DO WHILE binOrTxt$<>"B" AND binOrTxt$<>"T" 
    binOrTxt$=UCASE$(promptForAnyKey$("Binary or Text? (B/T)")) 
  LOOP

  LOCAL exportFilename$ = promptForText$("Export to file: ")
  IF exportFilename$ = "" THEN
    promptMsg "Export aborted.", 1
    EXIT SUB
  ENDIF

  IF MM.INFO(FILESIZE exportFilename$) <> -1 THEN 
    IF UCASE$(promptForAnyKey$("File exists. Overwrite? (Y/N)")) <> "Y" THEN
      promptMsg "Export aborted.", 1
      EXIT SUB
    ENDIF
  ENDIF

  promptMsg "Exporting...", 1

  IF binOrTxt$ = "B" THEN
    exportBin startAddrInt%, endAddrInt%, exportFilename$
  ELSE
    exportTxt startAddrInt%, endAddrInt%, exportFilename$
  ENDIF

  promptMsg "Done.       ", 1
END SUB

'Empty the keyboard input buffer
SUB emptyInputBuffer
  DO WHILE INKEY$ <> ""
  LOOP
END SUB

SUB deleteKeyHandler
  IF crsrFileOffset% < fileSize% THEN
    LOCAL index% = crsrFileOffset%

    promptMsg "Deleting...", 1

    DO WHILE index% < fileSize%-1
      writeByteBuf index%, readByteBuf$(index%+1)
      setModified index%, 1
      index% = index% + 1
    LOOP
    fileSize% = fileSize% - 1

    refreshPage

    promptMsg "", 0
  ENDIF
END SUB

'Backspace key handler.
SUB backSpace
  IF (crsrFileOffset% > 0) AND (crsrFileOffset% <= fileSize%) THEN
    LOCAL index% = crsrFileOffset% - 1

    promptMsg "Deleting...", 1

    DO WHILE index% < fileSize%-1
      writeByteBuf index%, readByteBuf$(index%+1)
      setModified index%, 1
      index% = index% + 1
    LOOP
    fileSize% = fileSize% - 1

    refreshPage

    promptMsg "", 0
  ENDIF

  'Unlike delete, backspace moves the cursor.
  IF (crsrFileOffset% > 0) THEN
    positionCursorAtAddr (crsrFileOffset% - 1), 1
  ENDIF
END SUB

SUB insertKeyHandler
  IF crsrFileOffset% < fileSize% THEN
    IF fileSize%+1 >= BUF_SIZE THEN
      promptMsg "Can not insert. File size limit reached.", 1
      EXIT SUB
    ENDIF

    promptMsg "Inserting...", 1

    LOCAL index% = fileSize%
    DO WHILE index% > crsrFileOffset%
      writeByteBuf index%, readByteBuf$(index%-1)
      setModified index%, 1
      index% = index% - 1
    LOOP

    writeByteBuf index%, CHR$(0)
    setModified index%, 1

    fileSize% = fileSize% + 1

    refreshPage

    promptMsg "", 0
  ENDIF
END SUB

SUB paasei
  PAGE COPY 0 TO 4
  PAGE WRITE 3  
  CLS RGB(BLACK)
  CIRCLE MM.HRES/2, MM.VRES/2, (MM.VRES/2)-10, 10, 1, RGB(BLACK), RGB(YELLOW)
  CIRCLE MM.HRES/2-100, MM.VRES/2-100, 40, 1, 1/2, RGB(BLACK), RGB(BLACK)
  CIRCLE MM.HRES/2+100, MM.VRES/2-100, 40, 1, 1/2, RGB(BLUE), RGB(BLUE)
  ARC MM.HRES/2, MM.VRES/2, (MM.VRES/4)-5, (MM.VRES/4)+5, 100, 260, RGB(BLACK)
  PAGE WRITE 5
  LOCAL scale!=0.1
  LOCAL newx%, newy%
  DO WHILE scale! < 1
    newx% = MM.HRES*(1 - scale!)/2
    newy% = MM.VRES*(1 - scale!)/2
    PAGE COPY 4 TO 5
    IMAGE RESIZE_FAST 0, 0, MM.HRES, MM.VRES, newx%, newy%, MM.HRES*scale!, MM.VRES*scale!, 3, 1
    PAGE COPY 5 TO 0, B
    scale! = scale! * 1.1
  LOOP
  PAGE COPY 4 TO 0, B
  PAGE WRITE 0
END SUB

SUB helpKeyHandler
  showHelpPopup
  LOCAL dummy$ = promptForAnyKey$("")
  removeHelpPopup
END SUB

SUB findKeyHandler  
  STATIC seq$(15) LENGTH 1
  STATIC modifiedsaved%(15)

  LOCAL inputStr$, fld$, anyKey$

  LOCAL ii%, matchCount%, seqLen%=0, direction% = 1
  LOCAL searchOffset% = crsrFileOffset%

  inputStr$ = promptForText$("find hex byte seq. (max. 16): ")

  FOR ii%=0 TO 15
    fld$ = FIELD$(inputStr$, ii%+1, " ,") 'Space and ',' are valid separators.
    IF fld$ <> "" THEN
      seq$(ii%) = ""
      ON ERROR SKIP 1
        seq$(ii%) = CHR$(EVAL("&H"+fld$))
      IF seq$(ii%) = "" THEN
        promptMsg "Invalid byte sequence. Aborting.", 1
        EXIT SUB
      ENDIF

      seqLen% = ii%+1
    endif
  NEXT ii%

  LOCAL done% = 0

  promptMsg "Searching...", 1

  DO WHILE NOT done%
    IF searchOffset% < 0 THEN
      IF UCASE$(promptForAnyKey$("Beginning of file reached. Wraparound? (Y/N)")) = "Y" THEN
        searchOffset% = fileSize%-seqLen%
        promptMsg "Searching...", 1
      ELSE
        done% = 1
        CONTINUE DO
      ENDIF
    ELSE IF searchOffset% + seqLen% >= fileSize% THEN
      IF UCASE$(promptForAnyKey$("End of file reached. Wraparound? (Y/N)")) = "Y" THEN
        searchOffset% = 0
        promptMsg "Searching...", 1
      ELSE
        done% = 1
        CONTINUE DO
      ENDIF
    ENDIF

    'The actual match loop. We have a match if we have seqLen consecutive byte matches.
    matchCount%=0
    FOR ii% = 0 TO seqLen%-1
      IF readByteBuf$(searchOffset% + ii%) = seq$(ii%) THEN
        matchCount% = matchCount%+1
      ELSE
        matchCount% = 0
      ENDIF
    NEXT ii%

    IF matchCount% = seqLen% THEN 'We have a match.
      positionCursorAtAddr(searchOffset%), 0
      
      'Ugly hack alert: temporarily abuse the modified array to highlight the found sequence
      FOR ii% = 0 TO seqLen%-1
        modifiedsaved%(ii%) = isModified%(crsrFileOffset%+ii%)
        setModified crsrFileOffset%+ii%, NOT modifiedsaved%(ii%)
      NEXT ii%
      'Sequence may be split across two rows.
      refreshRow rowStartAddr%(crsrFileOffset%), crsrRow%, 0
      refreshRow rowStartAddr%(crsrFileOffset%) + NUM_BYTES_PER_ROW%, crsrRow%+1, 0
      
      anyKey$ = UCASE$(promptForAnyKey$("n=next, p=previous, enter=done."))
      IF anyKey$ = "N" THEN
        direction% = 1
        searchOffset% = searchOffset%+direction%
        promptMsg "Searching...", 1
      ELSE IF anyKey$ = "P" THEN
        direction% = -1
        searchOffset% = searchOffset%+direction%
        promptMsg "Searching...", 1
      ELSE
        done% = 1
      ENDIF

      'Restore highlight
      FOR ii% = 0 TO seqLen%-1
        setModified crsrFileOffset%+ii%, modifiedsaved%(ii%)
      NEXT ii%
      refreshRow rowStartAddr%(crsrFileOffset%), crsrRow%, 0
      refreshRow rowStartAddr%(crsrFileOffset%) + NUM_BYTES_PER_ROW%, crsrRow%+1, 0

    ELSE 'No match. Move on one byte in selected direction.
      searchOffset% = searchOffset%+direction%
    ENDIF 
  LOOP

  promptMsg "", 0
END SUB

SUB fillKeyHandler
  LOCAL startAddrStr$ = promptForText$("Fill from Address: &H")
  LOCAL startAddrInt% = -1
  ON ERROR IGNORE 1
    startAddrInt% = EVAL("&H"+startAddrStr$)

  IF startAddrInt% < 0 THEN
    promptMsg "Start address invalid.", 1
    EXIT SUB
  ENDIF

  LOCAL endAddrStr$ = promptForText$("To Address: &H")
  LOCAL endAddrInt% = -1
  ON ERROR IGNORE 1
    endAddrInt% = EVAL("&H"+endAddrStr$)

  IF endAddrInt% < startAddrInt% THEN
    promptMsg "End address invalid.", 1
    EXIT SUB
  ELSE IF endAddrInt% >= BUF_SIZE% THEN
    promptMsg("End address is past file size limit of &H" + HEX$(endAddrInt%) + " bytes.", 1)
    EXIT SUB
  ENDIF

  LOCAL valueStr$ = promptForText$("Byte: &H")
  LOCAL valueInt% = -1 
  ON ERROR IGNORE 1
    valueInt% = EVAL("&H"+valueStr$)
  
  IF (valueInt% < 0) OR (valueInt% > 255) THEN
    promptMsg "Fill value invalid.", 1
    EXIT SUB
  ENDIF

  LOCAL index%

  'The strange case where the user requests to fill a block that's entirely located outside of the 
  'current file boundaries.
  IF startAddrInt% > fileSize% THEN
    'If there's a gap of more than one byte, ask to pad to cursor
    IF UCASE$(promptForAnyKey$("You are editing past the current end of the file. Pad with 0's to fill position? (Y/N)")) <> "Y" THEN
      EXIT SUB
    ENDIF
  
    FOR index% = fileSize% TO (startAddrInt%-1)
      writeByteBuf index%, CHR$(0)
      setModified index%, 1
    NEXT index%
  ENDIF

  promptMsg "Filling...", 1

  FOR index% = startAddrInt% TO endAddrInt%
    writeByteBuf index%, CHR$(valueInt%)
    setModified index%, 1
  NEXT index%

  promptMsg "Done.     ", 1

  'Adjust file size if we've grown the file by filling.
  IF endAddrInt% >= fileSize% THEN
    fileSize% = endAddrInt%+1
  ENDIF

  positionCursorAtAddr startAddrInt%, 0 'Move the cursor to the 1st byte of the fill block.
  refreshPage
END SUB

'CtrlP screenshot key handler.
SUB screenshot
  LOCAL screenshotFileName$ = promptForText$("Screenshot Filename: ")
  promptMsg "Saving...", 1
  SAVE IMAGE screenshotFileName$
  promptMsg "Screenshot saved.", 1
END SUB

'A modification is requested somewhere inside the hex table. The new nibble value is passed in as argument.
SUB editTable(nibble%)
  LOCAL triggerPageRefresh% = 0
  LOCAL index%

  'Note that there are no check against BUF_SIZE here. The idea is that the cursor can never get 
  'past the BUF_SIZE offset.

  'Allow to make modifications past the end of the file...
  IF crsrFileOffset% >= fileSize% THEN
    'If there's a gap of more than one byte, ask to pad to cursor
    IF crsrFileOffset% - fileSize% >= 1 THEN
      IF UCASE$(promptForAnyKey$("You are editing past the current end of the file. Pad with 0's to cursor position? (Y/N)")) = "Y" THEN
        triggerPageRefresh% = 1 'Trigger a full page refresh to make sure all the padding bytes show up.
      ELSE
        EXIT SUB
      ENDIF
    ENDIF
  
    'Pad with 0s from previous EOF position to current cursor position.
    FOR index% = fileSize% TO crsrFileOffset%
      writeByteBuf index%, CHR$(0)
      setModified index%, 1
    NEXT index%

    fileSize% = crsrFileOffset%+1
  ENDIF

  'Make the change
  LOCAL oldVal% = ASC(readByteBuf$(crsrFileOffset%))
  LOCAL newVal%
  IF crsrOnLeftNibble% <> 0 THEN
    newVal% = nibble%*16 OR (oldVal% AND 15)
  ELSE
    newVal% = (oldVal% AND &HF0) OR nibble%
  ENDIF

  LOCAL newValChar$ = CHR$(newVal%)
  writeByteBuf crsrFileOffset%, newValChar$
  setModified crsrFileOffset%, 1

  IF triggerPageRefresh% THEN
    refreshPage
  ELSE
    refreshRow rowStartAddr%(crsrFileOffset%), crsrRow%, 0
  ENDIF

  cursorRight 1 '1 indicates stay in block.
END SUB

'A modification is requested somewhere in the ASCII block.
SUB editASCblock(char$)
  LOCAL triggerPageRefresh%=0
  LOCAL index%

  'Same general outline as in the editTable sub above.

  IF crsrFileOffset% >= fileSize% THEN
    'If there's a gap of more than one byte, ask to pad to cursor
    IF crsrFileOffset% - fileSize% >= 1 THEN
      IF UCASE$(promptForAnyKey$("You are editing past the current end of the file. Pad with 0's to cursor position?")) = "Y" THEN
        triggerPageRefresh% = 1
      ELSE
        EXIT SUB
      ENDIF
    ENDIF
    
    FOR index% = fileSize% TO crsrFileOffset%
      writeByteBuf index%, CHR$(0)
      setModified index%, 1
    NEXT index%

    'Refresh page if there's a gap of more than one byte
    IF crsrFileOffset% - fileSize% >= 1 THEN
      refreshPage
    ENDIF

    fileSize% = crsrFileOffset%+1
  ENDIF

  writeByteBuf crsrFileOffset%, char$
  setModified crsrFileOffset%, 1

  IF triggerPageRefresh% THEN
    refreshPage
  ELSE
    refreshRow rowStartAddr%(crsrFileOffset%), crsrRow%, 0
  ENDIF

  cursorRight 1 '1 indicates stay in block.
END SUB

'Check for key presses
SUB checkKey
  STATIC nConsechomePresses% = 0, nConsecEndPresses% = 0
  LOCAL pressedKey$ = INKEY$

  IF pressedKey$ <> "" THEN
    'Remove any messages on the prompt line
    promptMsg "", 0

    IF pressedKey$ = CHR$(134) THEN
      nConsechomePresses% = nConsechomePresses% + 1
    ELSE
      nConsechomePresses% = 0
    ENDIF

    IF pressedKey$ = CHR$(135) THEN
      nConsecEndPresses% = nConsecEndPresses% + 1
    ELSE
      nConsecEndPresses% = 0
    ENDIF

    SELECT CASE ASC(pressedKey$)
      CASE 127 'Del
        deleteKeyHandler
      CASE 128 'Up Arrow
        cursorUp
      CASE 129 'Down Arrow
        cursorDown
      CASE 130 'Left Arrow
        cursorLeft
      CASE 131 'Right Arrow
        '0 indicates don't stay in block
        cursorRight 0
      CASE 132 'insert
        insertKeyHandler
      CASE 134 'home
        homeKeyHandler nConsechomePresses%
      CASE 135 'End
        endKeyHandler nConsecEndPresses% 
      CASE 136 'Page Up
        pageUp
      CASE 137 'Page Down
        pageDown
      CASE 145 'F1
        helpKeyHandler
      CASE 146 'F2
        saveKeyHandler
      case 147 'F3
        loadKeyHandler
      CASE 17 'ctrlQ
        quitKeyHandler
      CASE 5 'ctrlE
        exportKeyHandler
      CASE 15
        paasei
      CASE 6 'ctrlF
        fillKeyHandler
      CASE 7 'ctrlG
        gotoKeyHandler
      CASE 8 'BackSpace
        backSpace
      CASE 16 'CtrlP
        screenshot
      CASE 19 'CtrlS
        findKeyHandler
      CASE 20 'CtrlT
        toggleKeyHandler
      CASE ELSE
        'This is for the non-ctrl keys, i.e. the edits.
        IF cursorIsInTable%() <> 0 THEN
          LOCAL ucaseKey% = ASC(UCASE$(pressedKey$))
          'Convert characters A-F to values 10-15.
          IF ((ucaseKey%>=ASC("A")) AND (ucaseKey%<=ASC("F"))) THEN
            editTable ucaseKey% - ASC("A") + 10
          'Convert characters 0-9 to values.
          ELSEIF ((ucaseKey%>=ASC("0")) AND (ucaseKey%<=ASC("9"))) THEN
            editTable ucaseKey% - ASC("0")
          ENDIF
        ELSE 'ASC block
          editASCblock pressedKey$
        ENDIF
    END SELECT
  ENDIF
END SUB

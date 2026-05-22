# Custom NSIS configuration for canhrActi.
# Registers .agd files so double-clicking an ActiGraph data file opens canhrActi.

!macro customInstall
  WriteRegStr HKCU "Software\Classes\.agd" "" "canhrActi.AGD"
  WriteRegStr HKCU "Software\Classes\canhrActi.AGD" "" "ActiGraph Data File"
  WriteRegStr HKCU "Software\Classes\canhrActi.AGD\DefaultIcon" "" "$INSTDIR\canhrActi.exe,0"
  WriteRegStr HKCU "Software\Classes\canhrActi.AGD\shell\open\command" "" '"$INSTDIR\canhrActi.exe" "%1"'
!macroend

!macro customUnInstall
  DeleteRegKey HKCU "Software\Classes\canhrActi.AGD"
  DeleteRegValue HKCU "Software\Classes\.agd" ""
!macroend

$TIMESTAMP=$(get-date -f MM-dd-yyyy_HHmmss) 
$PREFIX=$(stack path --local-install-root)
$EXECUTABLES=".\executables"
$NAME=$(echo "commands-spiros-server") 
$DESKTOP="$(get-item env:HOMEPATH)\Desktop"

stack build

# PowerShell lacks (&&) 
if ($?) {
  xcopy /E /C /Y "$PREFIX\bin\$NAME.exe" "$EXECUTABLES\temporary\"
} 

if ($?) {
  xcopy /E /C /Y "$EXECUTABLES\temporary\$NAME.exe" ".\"
  xcopy /E /C /Y "$EXECUTABLES\temporary\$NAME.exe" "$EXECUTABLES"
  cp             "$EXECUTABLES\temporary\$NAME.exe" "$EXECUTABLES\$NAME-$TIMESTAMP.exe"
} 

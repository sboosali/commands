title commands-spiros-server 
for /l %%x in (1, 1, 1000) do (
  xcopy .stack-work\install\b45ea292\bin\commands-spiros-server.exe binaries\ /E /C /K /Y && stack build && stack exec -- commands-spiros-server
  pause
)

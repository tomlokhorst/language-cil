if [ $# -eq 1 ]; then
  rand="$RANDOM"
  runhaskell $1 > $rand.il &&
  ilasm2 $rand.il &&
  echo &&
  mono $rand.exe
  
  rm $rand.il
  rm $rand.exe
else
  echo "Usage:   ./run-example.sh FILE"
  echo "Note: Mono should be installed and in PATH"
fi


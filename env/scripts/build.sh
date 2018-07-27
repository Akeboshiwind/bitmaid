echo [LOG] Running \'lein uberjar\'
lein uberjar
echo [LOG] Copying jar file
cp ../target/uberjar/bitmaid*standalone.jar ./bitmaid.jar
echo [LOG] Finished!

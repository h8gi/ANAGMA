#ANAGMA
enigmaのシミュレータです  
chicken-schemeを使ってます  
cscでコンパイルしてください  

##Usage
-kオプションが取る3つの数字はANAGMA仮想ローターの初期位置を決めます  
-kオプションがない場合、初期位置はランダムです  
-eオプションはワンライナーの実行です  
-fオプションの後にはファイル名が続きます  
-rオプションでread eval print loop がスタートします  

``` bash
	anagma [OPTION ...] [FILENAME | STRING]  
    -e, --exec string  : string passed in  
    -f, --file file    : filename passed in  
    -h, --help         : print this help message  
    -k, --keys keys    : keys is init string of 3 rotors  
    -r, --repl         : start anagma repl  
    -v, --version      : print the anagma version  
```
###example
`anagma -k "0 0 0" -e "hello world"` => `MTWPZPRWHB`  
`anagma -k "0 0 0" -e "MTWPZPRWHB"` => `HELLOWORLD`  

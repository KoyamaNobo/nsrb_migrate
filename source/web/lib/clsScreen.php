<?php
//テスト作成 日進ゴム　ソース解析用
//author koyama
//create 2013.09.18
class clsScreen{
	public $intRecNum = 24;              //行数
	public $intFieNum = 80;              //列数
	public $strScreenReset = 0;         //リセットする必要ありのフラグ
	public $arrScreenStr = array();      //行数分clsLineを格納
	public $oLog;
	public $execErrorArray = array();
	private $intCurrentEchoLine = 1;      //shからechoで送られてくる文字をどの行に出力するか

	function __construct(){
		require_once('./lib/log.php');
		require_once('./lib/clsLine.php');
		$this->strScreenReset = 1;
		$this->oLog = New Log('');
		$this->lineReset();
	}

	//エラー配列をwebの画面表示形式に変換して返す
	//author :koyama
	//date   :20170207
	function exchengeFormatStringFromArray(){
		$stringmaxlen = 50;
		$returnString = '';
		foreach($this->execErrorArray as $error){
			$returnString .= preg_replace("/ Error /","", $error);
			// $this->oLog->error(":[web module]:Error message :".$error .':'.__FILE__.':'.__LINE__);
			//画面表示の長さの最大値
			if(strlen($returnString) > $stringmaxlen){
				$returnString = mb_strimwidth($returnString,0,$stringmaxlen-1,"...");
				break;
			}
		}
		return $returnString;
	}

	//画面をclear
	//IN：なし
	//property : arrScreenStr,intCurrentEchoLine,strScreenResetを初期化
	//author :koyama
	//date   :20140826
	function lineReset(){
		if($this->strScreenReset === 1){
			for($i = 1;$i <= $this->intRecNum;$i++){
				$temp = New clsLine($i);
				$this->arrScreenStr[$i] = $temp;
			}
			$this->intCurrentEchoLine = 1;
			$this->strScreenReset = 0;
		}
	}

	function lineAreaReset($fromLine,$toLine){
		for($i = (int)$fromLine;$i <= (int)$toLine;$i++){
			$temp = New clsLine($i);
			$this->arrScreenStr[$i] = $temp;
		}
	}

	//
	function lineAreaResetData($fromLine,$toLine){
		$fromLine = (int)$fromLine;
		if((int)$toLine == 0){
			$toLine = (int)$fromLine;
		}else{
			$toLine = (int)$toLine;
		}
		for($i = (int)$fromLine;$i <= (int)$toLine;$i++){
			foreach($this->arrScreenStr[$i]->arrLineElem as $key => $elem){
				if($elem->dataType === $elem->arrKind[1]){
					unset($this->arrScreenStr[$i]->arrLineElem[$key]);
				}
			}
		}
	}

	//HTMLかされたものを各要素に詰め直す関数を作りなおす
	function screenParse($htmlText){
		echo $htmlText;
	}

	//HTMLへ送るための変換
	//IN : screenLang->画面定義で書かれた画面表示
	//property:arrScreenStr[?]を変更
	//要素幾つかをparseし、現在のものに追加する様に機能改善
	function elemParse($screenLang){
		$arrResLine =array();
		$arrResLine = explode ("\n",$screenLang);
		foreach($arrResLine as $commLine){
			if(preg_match('/^\*/',$commLine)){
				continue;
			}
			$arrResComm = preg_split('/(_\s|_$)/',$commLine);
			foreach($arrResComm as $commTerm){
				//中身のテキストを入れ替え
				if(preg_match('/Please\s+(.+)$/',$commTerm,$matches)){
					$this->viewStrReplace(0,0,$matches[1]);
				}elseif(preg_match('/CON\s*\(\s*([0-9]+)\,\s*([0-9]+)\)(.+)$/',$commTerm,$matches)){
					//関数名(行,開始列,文字列)
					$this->lineReset();
					$this->viewStrReplace($matches[1],$matches[2],$matches[3]);
				}elseif(preg_match('/UND\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\).*/',$commTerm,$matches)){
				//アンダーラインを挿入
					//関数名(開始行,終了行,開始列,文字列)
					$this->lineReset();
					$this->viewUnderLine($matches[1],$matches[2],$matches[3],$matches[4]);
				}elseif(preg_match('/BOX\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\).*/',$commTerm,$matches)){
				//罫線(箱)を挿入
					//関数名(行,開始列,文字列)
					$this->lineReset();
					$this->viewBox($matches[1],$matches[2],$matches[3],$matches[4]);
				}elseif(preg_match('/OVE\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\).*/',$commTerm,$matches)){
				//罫線(上)を挿入
					//関数名(行,開始列,文字列)
					//echo $commTerm.PHP_EOL;
					$this->lineReset();
					$this->viewOver($matches[1],$matches[2],$matches[3],$matches[4]);
				}elseif(preg_match('/VER\s*\(\s*([0-9]+)\,?\s*([0-9]+)\)\s*(\(\s*([0-9]*)\,?\s*([0-9]*)\))?.*/',$commTerm,$matches)){
				//罫線(縦)を挿入
					//関数名(行,開始列,文字列)
					$this->lineReset();
					$this->viewVertical($matches);
				}elseif(preg_match('/INP\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\)\s+(.{1,20})\s+"([^"]+)".*/',$commTerm,$matches)){
					//関数名(行,開始列,文字列)
					$this->lineReset();
					$this->viewInput($matches[1],$matches[2],$matches[3],$matches[4],$matches[5],$matches[6]);
				}elseif(preg_match('/NIN\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\)\s+(.{1,20})\s+"([^"]+)".*/',$commTerm,$matches)){
					//関数名(行,開始列,文字列)
					$this->lineReset();
					$this->viewRemInput($matches[1],$matches[2],$matches[3],$matches[4],$matches[5],$matches[6]);
				}elseif(preg_match('/REV\s*\(\s*([0-9]*)\,?\s*([0-9]*)\)\s*\(\s*[0-9]*\,?\s*([0-9]*)\).*/',$commTerm,$matches)){
					//REVERSE
					//関数名(行,開始列,文字列)
					$this->reverse_add($matches[1],$matches[2],$matches[3]);
				}elseif(preg_match('/BLI\s*\(\s*([0-9]*)\,?\s*([0-9]*)\)\s*\(\s*[0-9]*\,\s*([0-9]*)\).*/',$commTerm,$matches)){
					//BLINK
					//関数名(行,開始列,文字列)
					$this->blink_add($matches[1],$matches[2],$matches[3]);
				}elseif(preg_match('/BUZ\s*\(\s*([0-9]*)\,?\s*([0-9]*)\)\s*\(\s*(.*)\,\s*([0-9]*)\)/',$commTerm,$matches)){
					//BUZZER
					//関数名(行,開始列,文字列)
					$this->viewBuzzer($matches[1],$matches[2],$matches[3],$matches[4]);
				}elseif(preg_match('/END.*/',$commTerm,$matches)){
					//ENDが来たら何もしない
					//関数名(行,開始列,文字列)

				}elseif(preg_match('/(SCREEN.*)/',$commTerm,$matches)){
					//SCREENが来たらスクリーンを切り替える？
					//関数名(行,開始列,文字列)
					//画面リセットをする必要があったらフラグを立てる
// 					$this->strScreenReset=1;
				}elseif(preg_match('/MOD.*/',$commTerm,$matches)){
					//MOD(MODE)が来たら何もしない
					//関数名(行,開始列,文字列)

				}elseif(preg_match('/AREA?\s*\(\s*([0-9]+),?([0-9]*)\s*\).*/',$commTerm,$matches)){
					//AREが来たら対象となっているAREAをリセット
					//関数名(行,開始列,文字列)
					$this->lineAreaReset($matches[1],$matches[2]);
				}elseif(preg_match('/(.*\s?\[ERR\]\s)(.*Error)(.*)/s',$commTerm,$matches)){
					//ERROR catch (JCL) format完
					//関数名(行,開始列,文字列)
					$this->execErrorArray[] = $matches[3];
				}elseif(preg_match('/(.*\s?\[ERR\]\s)(.*)/s',$commTerm,$matches)){
					//ERROR catch (JCL) format未完
					//関数名(行,開始列,文字列)
					$this->execErrorArray[] = $matches[2];
					$this->oLog->error(":[web module]:unfinished Error message :".print_r($matches,true).':'.__FILE__.':'.__LINE__);
				}elseif(preg_match('/(.*Error)(.*)/s',$commTerm,$matches)){
					//ERROR catch (LM)'/(.*\s?Error\s)(.*)/s'
					//関数名(行,開始列,文字列)
					$this->execErrorArray[] = $matches[2];
					$this->oLog->error(":[web module]:Error message :".$commTerm .':'.__FILE__.':'.__LINE__);
				}elseif(preg_match('/\*.*/',$commTerm,$matches)){
					//ENDが来たら何もしない
					//関数名(行,開始列,文字列)
				}else{
					//80カラムなので15以上あったら何かを出そうとしている
					if(preg_match('/.{15,}/',$commTerm)){
						$this->arrScreenStr[$this->intCurrentEchoLine]->setEchoText($commTerm);
						if($this->intCurrentEchoLine < 24){
							$this->intCurrentEchoLine++;
						}else{
							$this->intCurrentEchoLine = 1;
						}
					}
				}
			}
		}
	}

	//文字情報のセット
	function viewStrReplace($numLine,$numStartCol,$strText){
		//CREAR SCREENを受け取ったら一度リセット
		if(preg_match('/.*CLEAR\s+SCREEN.*/',$strText)){
			//画面リセットをする必要があったらフラグを立てる
			$this->strScreenReset=1;
			$this->lineReset();
		}elseif(preg_match('/.*CLEAR\s+LINE\s+TO\s+([0-9]+).*/',$strText,$matches)){
			$this->lineAreaReset($numLine,$matches[1]);
		}elseif(preg_match('/.*CLEAR\s+LINE.*/',$strText,$matches)){
			//その行のみ
			$this->lineAreaReset($numLine,$numStartCol);
		}elseif(preg_match('/.*CLEAR\s+DATA\s+TO\s+([0-9]+).*/',$strText,$matches)){
			//TOまで
			$this->lineAreaResetData($numLine,$matches[1]);
		}elseif(preg_match('/.*CLEAR\s+DATA.*/',$strText,$matches)){
			//本来カラムのところに終了を置いている
			$this->lineAreaResetData($numLine,$numStartCol);
		}else{
			if(isset($this->arrScreenStr[(int)$numLine])){
				$this->arrScreenStr[(int)$numLine]->setColText($strText,(int)$numLine,$numStartCol);
			}else{
				$this->oLog->error(":[web module]: can not display :Line[".$numLine."]:column[".$numStartCol."] text[".$strText."]".__FILE__.':'.__LINE__);
				//0行目の指定などはエラー出力ありで最終業として処理 これだと必要なものが消える
				// $this->arrScreenStr[$this->intRecNum]->setColText($strText,$this->intRecNum ,$numStartCol);
			}
		}
	}

	//REVERSEの属性を追加
	function reverse_add($numLine,$numStartCol,$numEndCol){
		if(isset($this->arrScreenStr[(int)$numLine])){
			$this->arrScreenStr[(int)$numLine]->setReverse((int)$numLine,$numStartCol,$numEndCol);
		}else{
			$this->oLog->error(": not found reverse target ".__FILE__.':'.__LINE__);
		}
	}

	//BLINKの属性を追加
	function blink_add($numLine,$numStartCol,$numEndCol){
		if(isset($this->arrScreenStr[(int)$numLine])){
			$this->arrScreenStr[(int)$numLine]->setBlink((int)$numLine,$numStartCol,$numEndCol);
		}else{
			$this->oLog->error(": not found reverse target ".__FILE__.':'.__LINE__);
		}
	}

	//アンダーラインの挿入
	function viewUnderLine($numStartLine,$numStartCol,$numEndLine,$numEndCol){
		if(empty($numEndLine)){
			$this->arrScreenStr[(int)$numStartLine]->setUnderLine((int)$numStartLine,$numStartCol,$numEndCol);
		} else {
			for($i=(int)$numStartLine;$i <= (int)$numEndLine;$i++){
				$this->arrScreenStr[$i]->setUnderLine($i,$numStartCol,$numEndCol);
			}
		}
	}

	//罫線(箱型)の生成
	function viewBox($numStartLine,$numStartCol,$numEndLine,$numEndCol){
		$numStartLine = (int)$numStartLine;
		$numStartCol  = (int)$numStartCol;
		$numEndLine   = (int)$numEndLine;
		$numEndCol    = (int)$numEndCol;
		$this->arrScreenStr[$numStartLine]->setBox((int)$numStartLine,(int)$numStartCol,(int)$numEndLine,(int)$numEndCol);
	}

	//罫線(上線)の生成
	function viewOver($numStartLine,$numStartCol,$numEndLine,$numEndCol){
		if(empty($numEndLine)){
			$this->arrScreenStr[(int)$numStartLine]->setOverLine($numStartLine,$numStartCol,$numEndCol);
		} else {
			for($i=(int)$numStartLine;$i <= (int)$numEndLine;$i++){
				$this->arrScreenStr[$i]->setOverLine($i,$numStartCol,$numEndCol);
			}
		}
	}

	//罫線(縦線)の生成
	function viewVertical($matches){
		$numStartLine = (int)$matches[1];
		$numStartCol  = (int)$matches[2];
		//必要ない？
		if(isset($matches[3])){
			$numEndLine   = (int)$matches[3];
		}else{
			$numEndLine = 0;
		}
		//([0-9]{2},[0-9]{2}) ([0-9]{2}この部分)
		if(isset($matches[4])){
			$numEndCol    = $matches[4];
		}else{
			$numEndCol = 0;
		}

		$this->arrScreenStr[$numStartLine]->setVertical($numStartLine,$numStartCol,$numEndCol);
	}

	//入力項目(INP)の挿入
	function viewInput($numStartLine,$numStartCol,$numEndLine,$numEndCol,$strTypeName,$strArgName){
		//いったん今まで出ていたものをすべて削除して次を表示
		foreach($this->arrScreenStr as $key => $writeline){
			$writeline->remInput();
		}

		//strTypeNameによってクラスを追加
		$strAddClass = $this->addClass($strTypeName);
		if(!isset($this->arrScreenStr[(int)$numStartLine])){
			$this->oLog->error(":[web module]:can not display:Line[".$numStartLine."]:column[".$numStartCol."] argName[".$strArgName."]".__FILE__.':'.__LINE__);
			return 1;
		}
		if(empty($numEndLine)){
			$this->arrScreenStr[(int)$numStartLine]->setInput($numStartCol,$numEndCol,$strArgName,$strAddClass);
		} else {
			$this->arrScreenStr[$numStartLine]->setInput($numStartCol,$numEndCol,$strArgName,$strAddClass);
		}
	}

	//入力項目(INP)の削除
	function viewRemInput($numStartLine,$numStartCol,$numEndLine,$numEndCol,$strTypeName,$strArgName){
		//対象のINPUTを削除
		if(empty($numEndLine)){
			$this->arrScreenStr[(int)$numStartLine]->remInput();
		} else {
			$this->arrScreenStr[$numStartLine]->remInput();
		}
	}


	//Buzzerの設定(beep音)
	function viewBuzzer($numStartLine,$numStartCol,$modalFlg,$soundsLength){
		$numStartLine  = (int)$numStartLine;
		$numStartCol   = (int)$numStartCol;
		$modalFlg      = $modalFlg;
		$soundsLength  = (int)$soundsLength;
		if($numStartLine == 0){
			$numStartLine = $this->intRecNum;
		}
		$this->arrScreenStr[$numStartLine]->setBuzzer($numStartLine,$numStartCol,$modalFlg,$soundsLength);
	}

	//inputタグに追加するクラスを判定
	function addClass($strTypeName){

		//strTypeNameによってクラスを追加
		$addFlg = false;
		$strAddClass = "";
		if($strTypeName == "9"){
			$strAddClass = $strAddClass . "NUMERC";
			$addFlg = true;
		}
		if($strTypeName == "S9"){
			if($addFlg){
				$strAddClass = $strAddClass . "_";
			}
			$strAddClass = $strAddClass . "SNUMERC";
			$addFlg = true;
		}
		if (preg_match("/^[9]+V[9]+$/", $strTypeName)) {
			$tmpArry = explode("V", $strTypeName);

			if($addFlg){
				$strAddClass = $strAddClass . "_";
			}
			$strAddClass = $strAddClass . "NUMERC".mb_strlen($tmpArry[0])."V".mb_strlen($tmpArry[1]);
			$addFlg = true;
		}
		if (preg_match("/^S[9]+V[9]+$/", $strTypeName)) {
			$tmp = substr($strTypeName, 1);
			$tmpArry = explode("V", $tmp);
			if($addFlg){
				$strAddClass = $strAddClass . "_";
			}
			$strAddClass = $strAddClass . "SNUMERC".mb_strlen($tmpArry[0])."V".mb_strlen($tmpArry[1]);
			$addFlg = true;
		}
		return $strAddClass;
	}
}

?>

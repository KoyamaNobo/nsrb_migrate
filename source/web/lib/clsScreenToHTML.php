<?php
//画面表示のparseとその表示データ
//author koyama
//create 20151005
class clsScreenToHTML{
	public $oLog;
	public $screen;
	public $lineIndex;

	function __construct(){
		require_once('./lib/log.php');
		require_once('./lib/clsScreen.php');
		$this->oLog = New Log('');
		$this->screen = New clsScreen();
		$this->lineIndex = 0;
	}

	function htmlEcho($fnameCtoP){
		$ErrorMax = 2;
		//実行系への入力
		if(!file_exists($fnameCtoP)){
			$this->oLog->error(__FILE__.':'.__LINE__.':write file_exists');
			//入力用のtmpファイルが存在しなければ強制終了
			return;
		}
		$fp = fopen($fnameCtoP,'w');
		if(!flock($fp,LOCK_EX)){
			//lockが取れなければ表示なしにする
			return;
		}
// 		$this->oLog->INFO(__FILE__.':'.__LINE__.':fopen');
		$wRes = fwrite($fp," ".PHP_EOL,2);
		foreach($this->screen->arrScreenStr as $key=>$view){
			//HTMLの仕様なはずだがFILEが正しく掛けないので最初スペース
			$wRes = fwrite($fp," ".$view->strStartTag.PHP_EOL,strlen($view->strStartTag.PHP_EOL) + 1);
			//echo表示がなければ中身の表示を試す
			if(empty($view->echoElem)){
				if(count($view->arrLineElem) > 0){
					foreach($view->arrLineElem as $key=>$elem){
						//行を削除した時に削除されている可能性ありｂ
						if(isset($elem)){
							//文字列の表示
							if($elem->type == 'TEX'){
								$wRes = fwrite($fp,$elem->getText().PHP_EOL);
							}
							//入力項目の表示
							if($elem->type == 'INP'){
								$wRes = fwrite($fp,$elem->getText().PHP_EOL);
							}
						}
					}
				}
			}else{
				$wRes = fwrite($fp,$view->echoElem.PHP_EOL);
			}
			$wRes = fwrite($fp,$view->strEndTag.PHP_EOL);
			if(count($view->arrLineElem) > 0){
				foreach($view->arrLineElem as $key=>$elem){
					//行を削除した時に削除されている可能性ありｂ
					if(isset($elem)){
						//罫線(下線)
						if($elem->type == 'UND'){
							$wRes = fwrite($fp,$elem->getText().PHP_EOL);
						}
						//罫線(箱)
						if($elem->type == 'BOX'){
							$wRes = fwrite($fp,$elem->getText().PHP_EOL);
						}
						//罫線(縦線)
						if($elem->type == 'VER'){
							$wRes = fwrite($fp,$elem->getText().PHP_EOL);
						}
						//罫線(上線)
						if($elem->type == 'OVE'){
							$wRes = fwrite($fp,$elem->getText().PHP_EOL);
						}
					}
				}
			}
		}
		if(count($this->screen->execErrorArray) > 0){
			$count = 0;
			//エラー文字列を変換して出力形式に
			$wRes = fwrite($fp,'<input type="hidden" class="error"  value="'. $this->screen->exchengeFormatStringFromArray() .'">');
		}
// 		$this->oLog->INFO(__FILE__.':'.__LINE__.':fclose');
		fclose($fp);
	}
}

?>

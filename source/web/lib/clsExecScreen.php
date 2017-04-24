<?php
//テスト作成 日進ゴム　隠しPG実行画面
//author koyama
//create 20150501
class clsExecScreen{
	private $intRecNum = 0;              //行数
	private $intFieNum = 80;              //列数
	private $strScreenRet = '';         //リセットする必要ありのフラグ
	public $arrScreenStr = array();      //行数分clsLineを格納
	public $oLog;
	public $execErrorArray = array();
	private $intCurrentEchoLine = 1;      //shからechoで送られてくる文字をどの行に出力するか
	
	function __construct($cmd,$oLog){
		require_once('./lib/log.php');
		require_once('./lib/clsLine.php');
		$this->strScreenReset = 1;
		$this->oLog = &$oLog;
		$cmd = $this->validateCmd($cmd);
		$this->oLog->info(__FILE__.':'.__LINE__.'[cmd]:'.$cmd);
		$this->strScreenRet = shell_exec(" ". $cmd . " 2>&1 ");
	}
	
	//HTMLへ送るための変換
	//IN : screenLang->画面定義で書かれた画面表示
	//property:arrScreenStr[?]を変更
	function screenParse($screenLang){
		$arrResLine =array();
		$arrResLine = explode ("\n",$this->strScreenRet);
		foreach($arrResLine as $commLine){
			$temp = New clsLine($this->intCurrentEchoLine);
			$this->arrScreenStr[$this->intCurrentEchoLine] = $temp;
			$this->arrScreenStr[$this->intCurrentEchoLine]->setEchoText($commLine);
			//現在の行数を格納
			$this->intCurrentEchoLine++;
			if($this->intCurrentEchoLine >= 24){
				$temp = New clsLine($this->intCurrentEchoLine);
				$this->arrScreenStr[$this->intCurrentEchoLine] = $temp;
				$this->arrScreenStr[$this->intCurrentEchoLine]->setEchoText(" .... more");
				break;
			}
		}
	}
	
	function validateCmd($cmd){
		$cmd = preg_replace('/(\s*ls)(\s([^-].*))?/','ls -alt',$cmd);
		return $cmd;
	}
}

?>
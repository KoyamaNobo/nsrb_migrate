<?php
//
// Processの情報格納(ユーザ単位,プロセス複数)
// author : koyama 
// create : 20161110
class clsSmartParse{
	public  $fp;
	private $sourceText;
	private $parseText;
	private $targetTablename;
	private $targetSubTablename;
	private $selectArray;
	private $oLog;
	
	//コンストラクタ(ファイル名あり)
	//ファイルより中身を読み出し中身をSMARTの基準により解析,必要な出力を
	//fileName:もとのファイル名
	function __construct($fileName){
		require_once('./lib/log.php');
		$this->oLog = New Log('');
		$this->oLog->error('[target file name]'.$fileName.':'.__FILE__.':'.__LINE__);
		$this->fp = fopen($fileName,'r');
		if(is_null($this->fp)){
			return ;
		}
		//プロパティ変数の初期設定
		$this->sourceText         = '';
		$this->parseText          = '';
		$this->targetTablename    = '';
		$this->targetSubTablename = array();
		$this->selectArray        = array();
		//処理
		while(!feof($this->fp)){
			$temp = fgets($this->fp);
			$temp = preg_replace('/[0-9]{5}$/','',$temp);
			$this->sourceText .= rtrim($temp);
		}
		$this->oLog->error('[target sourceText]'.$this->sourceText.':'.__FILE__.':'.__LINE__);
	}
	
	//echoでshから送られる文字情報
	function getParseText(){
		$this->parseText = '';
		$this->parseText = $this->setParseText();
		return $this->parseText;
	}
	
	//改行を除去済みのファイルの内容文字列を解析、DataUtilityで使う形式に変換された形の文字列を作成
	function setParseText(){
		$strParseTemp = preg_replace('/\\x{0f}/','"',$this->sourceText);
		if($strParseTemp != ''){
			//テーブル名のプロパティへのセット
			$this->setTargetTablename($strParseTemp);
			//画面表示項目を取得
			$this->setSelectField($strParseTemp);
		}
		$strParseTemp = $this->targetTablename.PHP_EOL.$this->getSelectField().PHP_EOL.$strParseTemp;
		return $strParseTemp;
	}
	
	//テーブル名の取得(Main,Sub)
	function setTargetTablename($subject){
		$matches = array();
		//MFI(Main File)のセット$matches[2]のボリューム名は現状無視
		preg_match('/MFI=([^,]+),([^_]*)/',$subject,$matches);
		$this->targetTablename = $matches[1];
		//SFIの取得                                                 TODO:SDIをArrayに格納するのは後で作成
		return ;
	}
	
	//画面表示する項目のセット
	function setSelectField($subject){
		$matches = array();
		//MFI(Main File)のセット$matches[2]のボリューム名は現状無視
		preg_match_all('/(SIM=[^_]*)/',$subject,$matches);
// 		$this->oLog->error('[target sourceText]'.print_r($matches[0],true).':'.__FILE__.':'.__LINE__);
		foreach($matches[0] as $simElem){
			$addElem = New clsSelectElem($simElem);
// 			$this->oLog->error('[target sourceText]'.print_r($simElem,true).':'.__FILE__.':'.__LINE__);
			//配列に要素を追加
			$this->selectArray[] = $addElem;
		}
		return ;
	}
	
	//画面表示する項目のセット
	function getSelectField(){
		$retVal = '';
		foreach($this->selectArray as $simElem){
			$this->oLog->error('[target sourceText]'.print_r($simElem,true).':'.__FILE__.':'.__LINE__);
			//配列に要素を追加
// 			$retVal .= $simElem->sourceText;
			if($simElem->fileClass == 0){
				$retVal .= '('.$simElem->column.','.$simElem->dataLength.','.$simElem->dataType.','.preg_replace('/"/',"'",$simElem->caption).')^' ;
			}
		}
		if(preg_match('/^$/',$retVal)){
			$retVal = preg_replace('/(^)$/','',$retVal);
		}
		return $retVal;
	}
}

//
// 画面出力項目の要素分解
// author : koyama 
// create : 20161110
class clsSelectElem{
	public $sourceText;
	public $elementName;             //1
	public $fileClass;               //2 0はメインファイル,7は定数
	public $column;                  //3
	public $dataType;                //4
	public $dataLength;                //5
	public $varidateFlg;             //11
	public $caption;                 //13
	public $operationFlg;            //14 操作区分 -> 0入力 1表示 2見出し => 操作するときにどこを画面表示するかなのでこのプログラムでは無視	
	
	function __construct($baseText){
		$this->sourceText = $baseText;
		$matches = array();
		preg_match('/\(([^)]*)\)/',$this->sourceText,$matches);
		$splitText = explode(',',$matches[1]);
		$this->elementName = $splitText[0];
		$this->fileClass   = $splitText[1];
		if($this->fileClass != 7){
			$this->column      = $splitText[2];
			$this->dataType    = $splitText[3];
			$this->dataLength  = $splitText[4];
			$this->caption     = $splitText[8];
		}else{
			$this->caption     = $splitText[12];
		}
	}
}
?>

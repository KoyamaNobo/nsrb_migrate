<?php
//
// Process�̏��i�[(���[�U�P��,�v���Z�X����)
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
	
	//�R���X�g���N�^(�t�@�C��������)
	//�t�@�C����蒆�g��ǂݏo�����g��SMART�̊�ɂ����,�K�v�ȏo�͂�
	//fileName:���Ƃ̃t�@�C����
	function __construct($fileName){
		require_once('./lib/log.php');
		$this->oLog = New Log('');
		$this->oLog->error('[target file name]'.$fileName.':'.__FILE__.':'.__LINE__);
		$this->fp = fopen($fileName,'r');
		if(is_null($this->fp)){
			return ;
		}
		//�v���p�e�B�ϐ��̏����ݒ�
		$this->sourceText         = '';
		$this->parseText          = '';
		$this->targetTablename    = '';
		$this->targetSubTablename = array();
		$this->selectArray        = array();
		//����
		while(!feof($this->fp)){
			$temp = fgets($this->fp);
			$temp = preg_replace('/[0-9]{5}$/','',$temp);
			$this->sourceText .= rtrim($temp);
		}
		$this->oLog->error('[target sourceText]'.$this->sourceText.':'.__FILE__.':'.__LINE__);
	}
	
	//echo��sh���瑗���镶�����
	function getParseText(){
		$this->parseText = '';
		$this->parseText = $this->setParseText();
		return $this->parseText;
	}
	
	//���s�������ς݂̃t�@�C���̓��e���������́ADataUtility�Ŏg���`���ɕϊ����ꂽ�`�̕�������쐬
	function setParseText(){
		$strParseTemp = preg_replace('/\\x{0f}/','"',$this->sourceText);
		if($strParseTemp != ''){
			//�e�[�u�����̃v���p�e�B�ւ̃Z�b�g
			$this->setTargetTablename($strParseTemp);
			//��ʕ\�����ڂ��擾
			$this->setSelectField($strParseTemp);
		}
		$strParseTemp = $this->targetTablename.PHP_EOL.$this->getSelectField().PHP_EOL.$strParseTemp;
		return $strParseTemp;
	}
	
	//�e�[�u�����̎擾(Main,Sub)
	function setTargetTablename($subject){
		$matches = array();
		//MFI(Main File)�̃Z�b�g$matches[2]�̃{�����[�����͌��󖳎�
		preg_match('/MFI=([^,]+),([^_]*)/',$subject,$matches);
		$this->targetTablename = $matches[1];
		//SFI�̎擾                                                 TODO:SDI��Array�Ɋi�[����̂͌�ō쐬
		return ;
	}
	
	//��ʕ\�����鍀�ڂ̃Z�b�g
	function setSelectField($subject){
		$matches = array();
		//MFI(Main File)�̃Z�b�g$matches[2]�̃{�����[�����͌��󖳎�
		preg_match_all('/(SIM=[^_]*)/',$subject,$matches);
// 		$this->oLog->error('[target sourceText]'.print_r($matches[0],true).':'.__FILE__.':'.__LINE__);
		foreach($matches[0] as $simElem){
			$addElem = New clsSelectElem($simElem);
// 			$this->oLog->error('[target sourceText]'.print_r($simElem,true).':'.__FILE__.':'.__LINE__);
			//�z��ɗv�f��ǉ�
			$this->selectArray[] = $addElem;
		}
		return ;
	}
	
	//��ʕ\�����鍀�ڂ̃Z�b�g
	function getSelectField(){
		$retVal = '';
		foreach($this->selectArray as $simElem){
			$this->oLog->error('[target sourceText]'.print_r($simElem,true).':'.__FILE__.':'.__LINE__);
			//�z��ɗv�f��ǉ�
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
// ��ʏo�͍��ڂ̗v�f����
// author : koyama 
// create : 20161110
class clsSelectElem{
	public $sourceText;
	public $elementName;             //1
	public $fileClass;               //2 0�̓��C���t�@�C��,7�͒萔
	public $column;                  //3
	public $dataType;                //4
	public $dataLength;                //5
	public $varidateFlg;             //11
	public $caption;                 //13
	public $operationFlg;            //14 ����敪 -> 0���� 1�\�� 2���o�� => ���삷��Ƃ��ɂǂ�����ʕ\�����邩�Ȃ̂ł��̃v���O�����ł͖���	
	
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

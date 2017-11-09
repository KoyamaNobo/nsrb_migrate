<?php
//�e�X�g�쐬 ���i�S���@�B��PG���s���
//author koyama
//create 20150501
class clsExecScreen{
	private $intRecNum = 0;              //�s��
	private $intFieNum = 80;              //��
	private $strScreenRet = '';         //���Z�b�g����K�v����̃t���O
	public $arrScreenStr = array();      //�s����clsLine���i�[
	public $oLog;
	public $execErrorArray = array();
	private $intCurrentEchoLine = 1;      //sh����echo�ő����Ă��镶�����ǂ̍s�ɏo�͂��邩
	
	function __construct($cmd,$oLog){
		require_once('./lib/log.php');
		require_once('./lib/clsLine.php');
		$this->strScreenReset = 1;
		$this->oLog = &$oLog;
		$cmd = $this->validateCmd($cmd);
		$this->oLog->info(__FILE__.':'.__LINE__.'[cmd]:'.$cmd);
		$this->strScreenRet = shell_exec(" ". $cmd . " 2>&1 ");
	}
	
	//HTML�֑��邽�߂̕ϊ�
	//IN : screenLang->��ʒ�`�ŏ����ꂽ��ʕ\��
	//property:arrScreenStr[?]��ύX
	function screenParse($screenLang){
		$arrResLine =array();
		$arrResLine = explode ("\n",$this->strScreenRet);
		foreach($arrResLine as $commLine){
			$temp = New clsLine($this->intCurrentEchoLine);
			$this->arrScreenStr[$this->intCurrentEchoLine] = $temp;
			$this->arrScreenStr[$this->intCurrentEchoLine]->setEchoText($commLine);
			//���݂̍s�����i�[
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
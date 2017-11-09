<?php
//�e�X�g�쐬 ���i�S���@�\�[�X��͗p
//author koyama
//create 2013.09.18
class clsScreen{
	public $intRecNum = 24;              //�s��
	public $intFieNum = 80;              //��
	public $strScreenReset = 0;         //���Z�b�g����K�v����̃t���O
	public $arrScreenStr = array();      //�s����clsLine���i�[
	public $oLog;
	public $execErrorArray = array();
	private $intCurrentEchoLine = 1;      //sh����echo�ő����Ă��镶�����ǂ̍s�ɏo�͂��邩

	function __construct(){
		require_once('./lib/log.php');
		require_once('./lib/clsLine.php');
		$this->strScreenReset = 1;
		$this->oLog = New Log('');
		$this->lineReset();
	}

	//�G���[�z���web�̉�ʕ\���`���ɕϊ����ĕԂ�
	//author :koyama
	//date   :20170207
	function exchengeFormatStringFromArray(){
		$stringmaxlen = 50;
		$returnString = '';
		foreach($this->execErrorArray as $error){
			$returnString .= preg_replace("/ Error /","", $error);
			// $this->oLog->error(":[web module]:Error message :".$error .':'.__FILE__.':'.__LINE__);
			//��ʕ\���̒����̍ő�l
			if(strlen($returnString) > $stringmaxlen){
				$returnString = mb_strimwidth($returnString,0,$stringmaxlen-1,"...");
				break;
			}
		}
		return $returnString;
	}

	//��ʂ�clear
	//IN�F�Ȃ�
	//property : arrScreenStr,intCurrentEchoLine,strScreenReset��������
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

	//HTML�����ꂽ���̂��e�v�f�ɋl�ߒ����֐������Ȃ���
	function screenParse($htmlText){
		echo $htmlText;
	}

	//HTML�֑��邽�߂̕ϊ�
	//IN : screenLang->��ʒ�`�ŏ����ꂽ��ʕ\��
	//property:arrScreenStr[?]��ύX
	//�v�f�����parse���A���݂̂��̂ɒǉ�����l�ɋ@�\���P
	function elemParse($screenLang){
		$arrResLine =array();
		$arrResLine = explode ("\n",$screenLang);
		foreach($arrResLine as $commLine){
			if(preg_match('/^\*/',$commLine)){
				continue;
			}
			$arrResComm = preg_split('/(_\s|_$)/',$commLine);
			foreach($arrResComm as $commTerm){
				//���g�̃e�L�X�g�����ւ�
				if(preg_match('/Please\s+(.+)$/',$commTerm,$matches)){
					$this->viewStrReplace(0,0,$matches[1]);
				}elseif(preg_match('/CON\s*\(\s*([0-9]+)\,\s*([0-9]+)\)(.+)$/',$commTerm,$matches)){
					//�֐���(�s,�J�n��,������)
					$this->lineReset();
					$this->viewStrReplace($matches[1],$matches[2],$matches[3]);
				}elseif(preg_match('/UND\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\).*/',$commTerm,$matches)){
				//�A���_�[���C����}��
					//�֐���(�J�n�s,�I���s,�J�n��,������)
					$this->lineReset();
					$this->viewUnderLine($matches[1],$matches[2],$matches[3],$matches[4]);
				}elseif(preg_match('/BOX\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\).*/',$commTerm,$matches)){
				//�r��(��)��}��
					//�֐���(�s,�J�n��,������)
					$this->lineReset();
					$this->viewBox($matches[1],$matches[2],$matches[3],$matches[4]);
				}elseif(preg_match('/OVE\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\).*/',$commTerm,$matches)){
				//�r��(��)��}��
					//�֐���(�s,�J�n��,������)
					//echo $commTerm.PHP_EOL;
					$this->lineReset();
					$this->viewOver($matches[1],$matches[2],$matches[3],$matches[4]);
				}elseif(preg_match('/VER\s*\(\s*([0-9]+)\,?\s*([0-9]+)\)\s*(\(\s*([0-9]*)\,?\s*([0-9]*)\))?.*/',$commTerm,$matches)){
				//�r��(�c)��}��
					//�֐���(�s,�J�n��,������)
					$this->lineReset();
					$this->viewVertical($matches);
				}elseif(preg_match('/INP\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\)\s+(.{1,20})\s+"([^"]+)".*/',$commTerm,$matches)){
					//�֐���(�s,�J�n��,������)
					$this->lineReset();
					$this->viewInput($matches[1],$matches[2],$matches[3],$matches[4],$matches[5],$matches[6]);
				}elseif(preg_match('/NIN\s*\(\s*([0-9]+)\,\s*([0-9]+)\)\s*\(\s*([0-9]*)\,\s*([0-9]+)\)\s+(.{1,20})\s+"([^"]+)".*/',$commTerm,$matches)){
					//�֐���(�s,�J�n��,������)
					$this->lineReset();
					$this->viewRemInput($matches[1],$matches[2],$matches[3],$matches[4],$matches[5],$matches[6]);
				}elseif(preg_match('/REV\s*\(\s*([0-9]*)\,?\s*([0-9]*)\)\s*\(\s*[0-9]*\,?\s*([0-9]*)\).*/',$commTerm,$matches)){
					//REVERSE
					//�֐���(�s,�J�n��,������)
					$this->reverse_add($matches[1],$matches[2],$matches[3]);
				}elseif(preg_match('/BLI\s*\(\s*([0-9]*)\,?\s*([0-9]*)\)\s*\(\s*[0-9]*\,\s*([0-9]*)\).*/',$commTerm,$matches)){
					//BLINK
					//�֐���(�s,�J�n��,������)
					$this->blink_add($matches[1],$matches[2],$matches[3]);
				}elseif(preg_match('/BUZ\s*\(\s*([0-9]*)\,?\s*([0-9]*)\)\s*\(\s*(.*)\,\s*([0-9]*)\)/',$commTerm,$matches)){
					//BUZZER
					//�֐���(�s,�J�n��,������)
					$this->viewBuzzer($matches[1],$matches[2],$matches[3],$matches[4]);
				}elseif(preg_match('/END.*/',$commTerm,$matches)){
					//END�������牽�����Ȃ�
					//�֐���(�s,�J�n��,������)

				}elseif(preg_match('/(SCREEN.*)/',$commTerm,$matches)){
					//SCREEN��������X�N���[����؂�ւ���H
					//�֐���(�s,�J�n��,������)
					//��ʃ��Z�b�g������K�v����������t���O�𗧂Ă�
// 					$this->strScreenReset=1;
				}elseif(preg_match('/MOD.*/',$commTerm,$matches)){
					//MOD(MODE)�������牽�����Ȃ�
					//�֐���(�s,�J�n��,������)

				}elseif(preg_match('/AREA?\s*\(\s*([0-9]+),?([0-9]*)\s*\).*/',$commTerm,$matches)){
					//ARE��������ΏۂƂȂ��Ă���AREA�����Z�b�g
					//�֐���(�s,�J�n��,������)
					$this->lineAreaReset($matches[1],$matches[2]);
				}elseif(preg_match('/(.*\s?\[ERR\]\s)(.*Error)(.*)/s',$commTerm,$matches)){
					//ERROR catch (JCL) format��
					//�֐���(�s,�J�n��,������)
					$this->execErrorArray[] = $matches[3];
				}elseif(preg_match('/(.*\s?\[ERR\]\s)(.*)/s',$commTerm,$matches)){
					//ERROR catch (JCL) format����
					//�֐���(�s,�J�n��,������)
					$this->execErrorArray[] = $matches[2];
					$this->oLog->error(":[web module]:unfinished Error message :".print_r($matches,true).':'.__FILE__.':'.__LINE__);
				}elseif(preg_match('/(.*Error)(.*)/s',$commTerm,$matches)){
					//ERROR catch (LM)'/(.*\s?Error\s)(.*)/s'
					//�֐���(�s,�J�n��,������)
					$this->execErrorArray[] = $matches[2];
					$this->oLog->error(":[web module]:Error message :".$commTerm .':'.__FILE__.':'.__LINE__);
				}elseif(preg_match('/\*.*/',$commTerm,$matches)){
					//END�������牽�����Ȃ�
					//�֐���(�s,�J�n��,������)
				}else{
					//80�J�����Ȃ̂�15�ȏ゠�����牽�����o�����Ƃ��Ă���
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

	//�������̃Z�b�g
	function viewStrReplace($numLine,$numStartCol,$strText){
		//CREAR SCREEN���󂯎�������x���Z�b�g
		if(preg_match('/.*CLEAR\s+SCREEN.*/',$strText)){
			//��ʃ��Z�b�g������K�v����������t���O�𗧂Ă�
			$this->strScreenReset=1;
			$this->lineReset();
		}elseif(preg_match('/.*CLEAR\s+LINE\s+TO\s+([0-9]+).*/',$strText,$matches)){
			$this->lineAreaReset($numLine,$matches[1]);
		}elseif(preg_match('/.*CLEAR\s+LINE.*/',$strText,$matches)){
			//���̍s�̂�
			$this->lineAreaReset($numLine,$numStartCol);
		}elseif(preg_match('/.*CLEAR\s+DATA\s+TO\s+([0-9]+).*/',$strText,$matches)){
			//TO�܂�
			$this->lineAreaResetData($numLine,$matches[1]);
		}elseif(preg_match('/.*CLEAR\s+DATA.*/',$strText,$matches)){
			//�{���J�����̂Ƃ���ɏI����u���Ă���
			$this->lineAreaResetData($numLine,$numStartCol);
		}else{
			if(isset($this->arrScreenStr[(int)$numLine])){
				$this->arrScreenStr[(int)$numLine]->setColText($strText,(int)$numLine,$numStartCol);
			}else{
				$this->oLog->error(":[web module]: can not display :Line[".$numLine."]:column[".$numStartCol."] text[".$strText."]".__FILE__.':'.__LINE__);
				//0�s�ڂ̎w��Ȃǂ̓G���[�o�͂���ōŏI�ƂƂ��ď��� ���ꂾ�ƕK�v�Ȃ��̂�������
				// $this->arrScreenStr[$this->intRecNum]->setColText($strText,$this->intRecNum ,$numStartCol);
			}
		}
	}

	//REVERSE�̑�����ǉ�
	function reverse_add($numLine,$numStartCol,$numEndCol){
		if(isset($this->arrScreenStr[(int)$numLine])){
			$this->arrScreenStr[(int)$numLine]->setReverse((int)$numLine,$numStartCol,$numEndCol);
		}else{
			$this->oLog->error(": not found reverse target ".__FILE__.':'.__LINE__);
		}
	}

	//BLINK�̑�����ǉ�
	function blink_add($numLine,$numStartCol,$numEndCol){
		if(isset($this->arrScreenStr[(int)$numLine])){
			$this->arrScreenStr[(int)$numLine]->setBlink((int)$numLine,$numStartCol,$numEndCol);
		}else{
			$this->oLog->error(": not found reverse target ".__FILE__.':'.__LINE__);
		}
	}

	//�A���_�[���C���̑}��
	function viewUnderLine($numStartLine,$numStartCol,$numEndLine,$numEndCol){
		if(empty($numEndLine)){
			$this->arrScreenStr[(int)$numStartLine]->setUnderLine((int)$numStartLine,$numStartCol,$numEndCol);
		} else {
			for($i=(int)$numStartLine;$i <= (int)$numEndLine;$i++){
				$this->arrScreenStr[$i]->setUnderLine($i,$numStartCol,$numEndCol);
			}
		}
	}

	//�r��(���^)�̐���
	function viewBox($numStartLine,$numStartCol,$numEndLine,$numEndCol){
		$numStartLine = (int)$numStartLine;
		$numStartCol  = (int)$numStartCol;
		$numEndLine   = (int)$numEndLine;
		$numEndCol    = (int)$numEndCol;
		$this->arrScreenStr[$numStartLine]->setBox((int)$numStartLine,(int)$numStartCol,(int)$numEndLine,(int)$numEndCol);
	}

	//�r��(���)�̐���
	function viewOver($numStartLine,$numStartCol,$numEndLine,$numEndCol){
		if(empty($numEndLine)){
			$this->arrScreenStr[(int)$numStartLine]->setOverLine($numStartLine,$numStartCol,$numEndCol);
		} else {
			for($i=(int)$numStartLine;$i <= (int)$numEndLine;$i++){
				$this->arrScreenStr[$i]->setOverLine($i,$numStartCol,$numEndCol);
			}
		}
	}

	//�r��(�c��)�̐���
	function viewVertical($matches){
		$numStartLine = (int)$matches[1];
		$numStartCol  = (int)$matches[2];
		//�K�v�Ȃ��H
		if(isset($matches[3])){
			$numEndLine   = (int)$matches[3];
		}else{
			$numEndLine = 0;
		}
		//([0-9]{2},[0-9]{2}) ([0-9]{2}���̕���)
		if(isset($matches[4])){
			$numEndCol    = $matches[4];
		}else{
			$numEndCol = 0;
		}

		$this->arrScreenStr[$numStartLine]->setVertical($numStartLine,$numStartCol,$numEndCol);
	}

	//���͍���(INP)�̑}��
	function viewInput($numStartLine,$numStartCol,$numEndLine,$numEndCol,$strTypeName,$strArgName){
		//�������񍡂܂ŏo�Ă������̂����ׂč폜���Ď���\��
		foreach($this->arrScreenStr as $key => $writeline){
			$writeline->remInput();
		}

		//strTypeName�ɂ���ăN���X��ǉ�
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

	//���͍���(INP)�̍폜
	function viewRemInput($numStartLine,$numStartCol,$numEndLine,$numEndCol,$strTypeName,$strArgName){
		//�Ώۂ�INPUT���폜
		if(empty($numEndLine)){
			$this->arrScreenStr[(int)$numStartLine]->remInput();
		} else {
			$this->arrScreenStr[$numStartLine]->remInput();
		}
	}


	//Buzzer�̐ݒ�(beep��)
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

	//input�^�O�ɒǉ�����N���X�𔻒�
	function addClass($strTypeName){

		//strTypeName�ɂ���ăN���X��ǉ�
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

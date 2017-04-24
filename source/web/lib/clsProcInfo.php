<?php
//
// Process�̏��i�[(���[�U�P��,�v���Z�X����)
// author : koyama
// create : 20151111
class clsProcInfo{
	private $procArray;
	private $userName = '';   //���Ƃ��瑀�삷�邱�Ƃ��l���ăI�u�W�F�N�g����


	//proc:���肷��L�[�ƂȂ�v���O������(runExec)
	//user:�������i��Ƃ��Ɏg��user��
	function __construct($proc,$user){
		require_once('./lib/log.php');
		$this->procArray = array();
		$this->userName  = $user;
// 		$retVal = shell_exec('ps aux|grep ' . $this->userName . " |awk '{printf \"%s||\",$2 }' ") ;
		$retVal = shell_exec('ps aux|grep "' . $this->userName . '" |grep "' . $proc . '"  |grep -v grep ') ;
		$mixedArray = explode(PHP_EOL,$retVal);
		foreach($mixedArray as $retLine){
			if(preg_match('/^\s*$/',$retLine)){
				continue;
			}
			$lineElem = preg_split('/\s+/',$retLine);
			$cls = New clsProcElem();
			$cls->setName($lineElem[10 + 2]);
			$cls->setPgOut($lineElem[count($lineElem) - 1]);
			$cls->setPgIn($lineElem[count($lineElem) - 2]);
			if(preg_match('/sh$/',$cls->getName())){
				$cls->setType('js');
			}else{
				$cls->setType('lm');
			}
			$this->procArray[] = $cls;
		}
	}

	//echo��sh���瑗���镶�����
	function getHTMLToProcs(){
		$retVal = '';
		if(count($this->procArray) > 0){
			$counter = 0;
			foreach($this->procArray as $putLine){
				$retVal .= '<form class="procItem" style="display:hidden" name="procinfo" method="get" action="">'.PHP_EOL;
				$retVal .= '    <input type="submit" name="procInfo" value="�ڑ�" readonly="readonly" />'.PHP_EOL;
				$retVal .= '    <input type="text" name="filename" value="'.$putLine->getName().'" readonly="readonly" />'.PHP_EOL;
				$retVal .= '    <input type="hidden" name="infname" value="'.$putLine->getPgOut().'" readonly="readonly" />'.PHP_EOL;
				$retVal .= '    <input type="hidden" name="outfname" value="'.$putLine->getPgIn().'" readonly="readonly" />'.PHP_EOL;
				$retVal .= '    <input type="text" name="typ" value="'.$putLine->getType().'" readonly="readonly" />'.PHP_EOL;
				$retVal .= '</form>'.PHP_EOL;
				$counter++;
			}
		}

		return $retVal;
	}

	function getCountProcArray(){
		return count($this->procArray);
	}
}

class clsProcElem{
	private $Name;
	private $strPgOut;
	private $strPgIn;
	private $strType;

	function __construct(){
		$this->Name = '';
		$this->strPgOut = '';
		$this->strPgIn = '';
	}

	//name
	function setName($str){
		$this->Name = $str;
	}

	function getName(){
		return $this->Name;
	}
	//pgout
	function setPgOut($str){
		$this->strPgOut = $str;
	}

	function getPgOut(){
		return $this->strPgOut;
	}
	//pgin
	function setPgIn($str){
		$this->strPgIn = $str;
	}

	function getPgIn(){
		return $this->strPgIn;
	}
	//type
	function setType($str){
		$this->strType = $str;
	}

	function getType(){
		return $this->strType;
	}
}
?>

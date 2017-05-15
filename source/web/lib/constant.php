<?php
///
///�ς�邱�Ƃ̂Ȃ��萔�𕪊�
///
define('HTML_CLASS_REVERSE','reverse');       //class�Ƃ��Ă�reverse�̎w��
define('PHP_CLASS_REVERSE','REV');            //PHP�ł̋敪�Ƃ��Ă�reverse�̎w��
define('HTML_CLASS_BLINK','blink');           //class�Ƃ��Ă�blink�̎w��
define('PHP_CLASS_BLINK','BLI');            //PHP�ł̋敪�Ƃ��Ă�reverse�̎w��

require_once('./lib/log.php');
class initConf{
	private $arrTopMenu;
	private $arrKind;
	private $arrColorMap;  //�\���p�̕�����̒�`
	private $intExtState;  //�@�\�g���̏�� 0:�ʏ� 1:FileExplorer�g��
	private $arrExplorerPath;  //FileExplorer�g���@�\�̑Ώۃp�X�z��
	private $strExplorerPath;  //FileExplorer�g���@�\�̑Ώۃp�X������

	function __construct(){
		$this->arrTopMenu  = array('imnu00','kmnu00','gmenu','hak00','koh00','kob00','mitu01','mitu03'
		                    ,'zai000','teg000','tama01','ryob01','ryob02');
		$this->arrKind     = array('PM','JS','LM','SM');
		$this->arrColorMap = array(
		                            '#131313'  => '��',
		                            '#FFFFFF'  => '��',
		                            '#808080'  => '�D',
		                            '#00FFA5'  => '��',
		                            '#DC143E'  => '��',
		                            '#0000CD'  => '��',
		                            '#FFA500'  => '��',
		                            '#00FFFF'  => '��',
		                            '#EE82EE'  => '��',
		                           );
		$this->arrExplorerPath = array('/var/local/DataExchange/','../tmp/');
		$this->strExplorerPath = $this->arrExplorerPath[0];
		$this->intExtState = 0;  //intExtState [0:������� 1:FileExplorer]
	}

	//�Y�������Ƃ��Đ��������Ă��̔ԍ��ɑΉ�����@�\����Ԃ�
	//IN :�Y�����̐���
	//OUT:�@�\��(JS,LM�Ȃ�)
	//author:koyama
	public function getKind($num){
		//�Y���������݂��Ȃ����0
		if(array_key_exists($num,$this->arrKind)){
			return $this->arrKind[$num];
		}else{
			return 0;
		}
	}

	//Explorer�p��Path��Ԃ�
	//IN :�Ȃ�
	//OUT:�t�@�C���p�X�̕�����
	//author:koyama
	public function setstrExplorerPath($index){
		$this->intExtState = $index;
		$this->strExplorerPath = $this->arrExplorerPath[$index];
	}

	//Explorer�p��Path��Ԃ�
	//IN :�Ȃ�
	//OUT:�t�@�C���p�X�̕�����
	//author:koyama
	public function getstrExplorerPath(){
		return $this->strExplorerPath;
	}

	//Explorer�p�̌��݃Z�b�g����Ă���@�\�̔ԍ���Ԃ�
	//IN :�Ȃ�
	//OUT:�@�\�̔ԍ�
	//author:koyama
	public function getintExtState(){
		return $this->intExtState;
	}

	//Explorer�p�Ō��݂̋@�\�������Ɏ��̃y�[�W�J�ڐ��Ԃ�
	//IN :�Ȃ�
	//OUT:�t�@�C�����̕�����
	//author:koyama
	public function getFileExpDetail($location){
		$retVal = '';
		switch ($location){
		case 'index':
			$retVal = 'FileExplorerDetail.php';
			break;
		case 'detail':
			$retVal = 'FileExplorer.php';
			break;
		default:
			$retVal = 'FileExplorer.php';
		}
		return $retVal;
	}

	//�Y�����ɓ�����F��16�i���܂��͉p�������ē��{�ꖼ��Ԃ�
	//IN :�F��16�i���܂��͉p��(#FFFFFF,white�Ȃ�)
	//OUT:���{�ꖼ(���Ȃ�)
	//author:koyama
	public function getJapanColorName($cn){
		//�Y���������݂��Ȃ����0
		if(array_key_exists($cn,$this->arrColorMap)){
			return $this->arrColorMap[$cn];
		}else{
			return '�Ȃ�';
		}
	}

	public function getColorNameList(){
		return $this->arrColorMap;
	}

	public function setDefaultValue($val,$name){
		if(!is_null($val) && !empty($val)){
			return $val;
		}else{
			//�f�t�H���g�̒l��Ԃ�
			$keys=array();
			switch($name){
				case 'reverse_font_color':
					$keys = array_keys($this->arrColorMap);
					return $keys[1];
				case 'reverse_bg_color':
					$keys =  array_keys($this->arrColorMap);
					return $keys[5];
				case 'bg_color':
					$keys =  array_keys($this->arrColorMap);
					return $keys[0];
				case 'font_color':
					$keys =  array_keys($this->arrColorMap);
					return $keys[3];
				case 'font_size':
					return MIN_F_SIZE + ((MAX_F_SIZE - MIN_F_SIZE) / 2);
			}
			return $keyname;
		}
	}

	//���O�C���ł��邩�ǂ����̃`�F�b�N
	//in:$acc->���[�Uid(���O�C���A�J�E���g��),$pass->���O�C���p�X���[�h,
	//   $table->���[�U���e�[�u����result,$message->���^�[�����郁�b�Z�[�W�i�[
	public function loginCheck($acc, $pass ,&$message){
		$result     = false;
		$user_id    = '';
		$ser_name   = '';
		$password   = '';
		$def_name   = '';
		$font_size  = '';
		$font_color = '';
		$bg_color   = '';
		$pg_names   = array();
		$print_id   = '';
		$i = 0;        //foreach�̍ۂ̃��[�v�ϐ�

		try{
	//		$dbh = new PDO("mysql:dbname=". DB_NAME .";host=" . DB_HOST . ";port=". DB_PORT, DB_USER, '');
	 		$dbh = new PDO("mysql:dbname=". DB_NAME .";host=" . DB_HOST . ";port=". DB_PORT, DB_USER, DB_PASS);
		}catch (Exception $e) {
			session_destroy();
			$mess = 'Can not Connect Database <br />'.$e->getMessage();
			$mess = urlencode($mess);
			header('Location: ./sessDestroy.php?mess='.$mess);
		}
		//�����permission�x�[�X�Ń��O�C���\���X�g������
		//SELECT * FROM M_USER MU join M_PG MP on MU.permission NOT like REPLACE(REPLACE(rpad(MP.permission,14,'0'),'0','_'),'1','0') order by user_id;
		$sql  = ' SELECT user_id,user_name,password,DEF_PG.pg_name as def_name,MP.permission as permission,print_id, MP.pg_name as pg_name ';
		$sql .= ' ,font_size,font_color,bg_color,reverse_font_color,reverse_bg_color ';
		$sql .= ' FROM M_USER MU ';
		$sql .= ' INNER JOIN M_PG DEF_PG ';
		$sql .= ' ON MU.pg_id = DEF_PG.pg_id ';
		$sql .= ' INNER JOIN M_PG MP ';
		$sql .= ' ON MU.permission NOT like REPLACE(REPLACE(rpad(MP.permission,CHAR_LENGTH(MP.permission),"0"),"0","_"),"1","0") ';
		$sql .= ' WHERE user_name=:user_name AND password=:password';
		$sth = $dbh->prepare($sql);
		$sth->bindParam('user_name',$acc);
		$sth->bindParam('password',$pass);
		$sth->execute();
	// 	$userTable = $sth->fetchAll();
	// 	foreach($userTable as $user){
	// echo $sql;
		while($user = $sth->fetch(PDO::FETCH_ASSOC)){
			$user_id   = $user['user_id'];
			$user_name = $user['user_name'];
			$password  = $user['password'];
			$def_name  = $user['def_name'];
			array_push($pg_names, $user['pg_name']);
			$print_id  = $user['print_id'];
			$font_size = $this->setDefaultValue($user['font_size'],'font_size');
			$font_color= $this->setDefaultValue($user['font_color'],'font_color');
			$bg_color  = $this->setDefaultValue($user['bg_color'],'bg_color');
			$reverse_font_color= $this->setDefaultValue($user['reverse_font_color'],'reverse_font_color');
			$reverse_bg_color  = $this->setDefaultValue($user['reverse_bg_color'],'reverse_bg_color');
			$result = true;
			$i++;
	// 		print_r($user);
			$message .= 'user_id'.$user_id.' user_name'.$user_name;
		}

		if(!empty($user_id)){
	//		$message .= 'user_id'.$user_id.'user_name'.$user_name.'password'.$password.'def_name'. $def_name.'pg_name'.$pg_names.'print_id'. $print_id;;
			$_SESSION['user_id']   = $user_id;
			$_SESSION['user_name'] = $user_name;
			$_SESSION['password']  = $password;
			$_SESSION['def_name']  = $def_name;
			$_SESSION['pg_name']   = $pg_names;
			$_SESSION['print_id']  = $print_id;
			$_SESSION['font_size'] = $font_size;
			$_SESSION['font_color'] = $font_color;
			$_SESSION['bg_color']   = $bg_color ;
			$_SESSION['reverse_font_color'] = $reverse_font_color;
			$_SESSION['reverse_bg_color']   = $reverse_bg_color ;
		}else{
			$result = false;
		}



		if(!$result){

			$message .= ' ���O�C���Ɏ��s���܂����B';
		}

		return $result;
	}

}

////////////////////////////////////////////////////////////////////////////////���ʊ֐�
//�G���[�֐�
function errOutPut($mess){
	//�N���X�̃C���X�^���X���i���O�j
	$oLog      = New Log('');
	$oLog->info(__FILE__.':'.__LINE__.'[mess]:'.($mess));
	//�G���[���b�Z�[�W�󂯓n��
	header('Location: ./sessDestroy.php?mess='.$mess);
	exit;
}
function sessionCheck(&$message){
	$checkKeyArray = array('user_id'
	,'user_name','password','def_name'
	//,'pg_name'                                 //pgname �͔z��Ȃ̂ł݂Ȃ��ł���
	,'print_id','font_size'
	,'font_color','bg_color','reverse_font_color'
	,'reverse_bg_color');
	foreach($checkKeyArray as $key){
		if(!array_key_exists($key,$_SESSION)){
			return false;
		}

		if(empty($_SESSION[$key])){
			return false;
		}
	}
	return true;
}

//���s�t�@�C���̖��O���w�肵�Ă��̖��O���K��
//in:���s�t�@�C���̖��O(�p�X)
//out:���̎��s�t�@�C���̖��O(��������ꍇ�͐�Ɍ�����������)
function getExecTitle($strPs){
	$retVal='';

	return $retVal;
}

function changeHttps(){
	if (empty($_SERVER['HTTPS'])) {
		header("Location: https://{$_SERVER['HTTP_HOST']}{$_SERVER['REQUEST_URI']}");
		exit;
	}
}

//�v���Z�X�I�����̖߂�Ή�
//�v���Z�XID����v����v���Z�X�̑��݊m�F
function Unix_IsPidExisted($pid,$infname,$outfname,$log)
{
	$ret = false;
	//�v���Z�XID�͌J��Ԃ��g�p����邽��
	//�C���v�b�g�t�@�C���ƃA�E�g�v�b�g�t�@�C�����m�F�R�}���h�Ɋ܂߂�
	$cmd = "/bin/ps -p " .$pid. " -o pid ";
	$fp  = popen($cmd, "r");
	while( ($line = fgets($fp)) != false ){
		if( intval(trim($line)) == $pid ){
			$ret = true;
			break;
		}
	}
	pclose($fp);
	return $ret;
}

//�X�e�[�^�X�o�[�Ή�
//$pid����Ɏq�v���Z�X�̏�ԁi�W���u���E�X�e�[�^�X�EPID�j�擾
function getProcessIdStatus($pid,$log){

	$statArray = array(
		'pid'        => '',
		'stat'       => '',
		'jobname'    => '',
		'command'    => ''
	);

	$processTree = array();

	//�q�̃v���Z�X�����ׂĎ擾
	getLastProcessIds($pid,$pid,$processTree,"children");

	//�q�̃v���Z�XID�i�����j�q�̃v���Z�XID�݂̂ɂ��Ď擾
	$targetPids = array();
	foreach($processTree as $process){
		$tmpArray = split(",",$process);
		array_push($targetPids,$tmpArray[count($tmpArray)-1]);
	}

	//�z��t���璲�ׂ�i�q�v���Z�X���璲�ׂ�j
	//../exec/�����������ԁi�W���u���E�X�e�[�^�X�EPID�j���擾
	//../job/���������疳�����Łi�W���u���E�X�e�[�^�X�EPID�j���擾���ďI��
	$breakflg = 0;
	$countArray = count($targetPids);
	for ($i = $countArray-1; $i >= 0; $i--) {
		$process = shell_exec("ps aux |awk 'match($2,/". $targetPids[$i] ."/) {printf \"%s||%s||%s||%s||\",$2,$8,$11,$12 }' ");
		$setflg=0;
		if(!empty($process) && $process != "" ){
			$tmpArray = explode("||",$process);
			$loopcount = 0;
			foreach($tmpArray as $tmp){
				switch ($loopcount) {
					case 0:
						$statArray['pid']     = $tmp;
						break;
					case 1:
						if(preg_match("/T/",$tmp)){
							$tmp="P";
						}else{
							$tmp="A";
						}
						//��Ԏq���ǂ̏�Ԃ��Ŕ��f
						if($statArray['stat'] === ""){
							$statArray['stat']    = $tmp;
						}
						break;
					case 2:
						if (preg_match("[^../exec/]", $tmp)) {
							$statArray['jobname'] = str_replace("../exec/", "", $tmp);
						}
						break;
					case 3:
						//�u../job/�v�Ŏ擾�ł����Ƃ��͂�������g�p����
						if($statArray['jobname'] != ""){
							break;
						}
						if (preg_match("[^../job/]", $tmp)) {
							$statArray['jobname'] = str_replace("../job/", "", $tmp);
							$statArray['jobname'] = strtoupper(substr($statArray['jobname'], 0,-3));
						}
						break;
				}
				$loopcount++;
				if($loopcount >= 4){
					break;
				}
			}
		}
	}
	return $statArray;
}

//�X�e�[�^�X�o�[�Ή�
//�ċA�I�Ɏq�v���Z�X��ID���擾
//      $arg��children�̂Ƃ�
//          �߂�l��pid�݂̂�Ԃ�
//          ��jprocessTree[0]��123,457
//              processTree[1]��123,457,852
//              processTree[2]��123,745
//      $arg��leaf�̂Ƃ�
//          �߂�l�͗t��pid�Ƃ��̐e��pid���J���}�q���ŕԂ�
//          ��jprocessTree[0]��123,457,852 ���t��852
//              processTree[1]��123,745 ���t��745
function getLastProcessIds($pid,$str,&$processTree,$arg){
	//�q�̃v���Z�XID�擾
	$tmppid = shell_exec("ps  --no-header --ppid " . $pid . " |awk '{printf \"%s\\n\",$1}' ");
	$tmppid = explode("\n",$tmppid);
	if(!empty($tmppid[0]) && $tmppid[0] != "" ){
		if($arg=="children"){
			array_push($processTree,$str.",".$tmppid[0]);
		}
		getLastProcessIds($tmppid[0], $str.",".$tmppid[0],$processTree,$arg );
	}else{
		//����ȏ�q�����Ȃ����ɒʂ�
		if($arg=="leaf"){
			array_push($processTree, substr($str, (strrpos($str,',') + 1) ));
		}
	}
}
?>

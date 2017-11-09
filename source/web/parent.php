<?php
define('CSCREEN','/clear\s+screen/i');
require_once('./lib/log.php');
if(isset($argv[1])){
	$cmd = $argv[1];
}else{
	exit;
}

//file�폜�̂��߂ɗ����K�v
$fnameCtoP = $argv[(count($argv) - 1)];
$fnamePtoC = $argv[(count($argv) - 2)]; //�������ݗp�t�@�C��
$oLog = New Log('');
$efilename = DATA_SAVE_PASS.$argv[(count($argv) - 3)]."error.log"; 
$descriptorspec = array(
   0 => array("pipe", "r"),  // stdin �́A�q�v���Z�X���ǂݍ��ރp�C�v�ł��B
   1 => array("pipe", "w"),  // stdout �́A�q�v���Z�X���������ރp�C�v�ł��B
   2 => array("file", $efilename, "w") //�G���[�̓t�@�C���ɏ������݂܂��B
);

$strWrite = '' ;//���̓f�[�^�̊i�[
$strRead   = '' ;//output�p�f�[�^�̊i�[
$readStatus =  '';

// echo 'test:'.$cmd." ";
// print_r($argv);

// �G���[�t�@�C���쐬
touch( $efilename );


$startTime = 0;
//unix�^�C���X�^���v�ɕύX(�q�v���Z�X���ł����Ƃ��ɊJ�n����+(EXEC_LIVE * 60)�ŏ���������)
$t = new clsProcessStruc;
$t->process = proc_open($cmd,$descriptorspec,$t->pipes);
//fgets���Ōł܂��Ă��܂�����
stream_set_blocking($t->pipes[0],0);
stream_set_blocking($t->pipes[1],0);
if(is_resource($t->process)){
// 			stream_set_blocking($t->pipes[0],0);
// 			stream_set_blocking($t->pipes[1],0);
	
	$oLog->info("success".__FILE__.__LINE__);
	$startTime = time() + (EXEC_LIVE * 60);
	while(1){
		$exitWaitFlg = false;
		//�ŏ��ɏ���������҂��Ă���X�^�[�g
		usleep(EXEC_SLEEP);
		
		//�v���Z�X�̏�Ԃ�false�̏ꍇ�I��
		$procStatus = proc_get_status($t->process);

		//���s�n�ւ̓���
		if(!file_exists($fnamePtoC)){
			$oLog->info(__FILE__.':'.__LINE__.'resource1:');
			//���͗p��tmp�t�@�C�������݂��Ȃ���΋����I��
			break;
		}
		$fp = fopen($fnamePtoC,'r+');
		$wRes = 0;
		if($fp){
			stream_set_blocking($t->pipes[0],1);
			$strWrite = fgets($fp,1024);
			if(!empty($strWrite)){
				fflush($t->pipes[0]);
				$wRes = fwrite($t->pipes[0],$strWrite);
				//���ߑł��œ��͂����玟��read�܂ŏ����҂�
				usleep(500);
			}
			if($wRes != 0){
				ftruncate($fp,0);
			}else{
			}
			unset($wRes);
			fclose($fp);
		}else{
			//�t�@�C��������ɊJ���Ȃ��Ƃ��͋����I��
			$oLog->info(__FILE__.':'.__LINE__.' file:open error');
			break;
		}
		
		//���s�n����̑΂��Ă̏o��
		//���s�n�̌��ʂ��c��ǂꂭ�炢���邩�̃X�e�[�^�X���擾
		//�c��̓ǂݍ��݃f�[�^���Ȃ���Γǂݍ��܂Ȃ�
//  		$oLog->info(__FILE__.':'.__LINE__.'readed:'.$startTime);
		while(!empty($strRead = fgets($t->pipes[1],1024))){
			//���s�݂̂͏o�͂��Ȃ�
			if(strlen($strRead) > 0){
				echo $strRead;
				//���s�͏o�͂Ɋ܂܂�Ă��邽�ߕs�v comment koyama 20150713
// 				echo $strRead.PHP_EOL;
			}
		}
		//�I���\�莞�Ԃ��߂����狭���I��
		if($startTime < time()){
			break;
		}
		
		//�v���Z�X�I�����̖߂�Ή�
		//�v���Z�X�̏�Ԃ�false�̏ꍇ�I��
		if(!$procStatus["running"]){
				$exitWaitFlg = true;
				//�����҂��Ă��Ȃ��ƃG���[���b�Z�[�W����蓦��
				usleep(EXEC_SLEEP);
		}
		//���if��1��ȏ�ʂ��Ă�����
		if($exitWaitFlg === true){
			break;
		}
	}
	proc_close($t->process);
	//�쐬���ꂽ�v���Z�X�Ǘ��p�̃t�@�C���͍�������x���ŏ���(comment koyama 20150707)
// 	unlink($fnameCtoP);
// 	unlink($fnamePtoC);
}



//�v���Z�X�̊Ǘ��p�̃N���X
//author koyama
class clsProcessStruc {
	public $process;
	public $pipes;
	
	function __construct(){
		$this->process = 0;
		$this->pipes = (array)NULL;
	}
	
	function getAlive(){
		var_dump($this);
	}
}

?> 
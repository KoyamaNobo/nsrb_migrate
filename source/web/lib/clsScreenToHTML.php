<?php
//��ʕ\����parse�Ƃ��̕\���f�[�^
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
		//���s�n�ւ̓���
		if(!file_exists($fnameCtoP)){
			$this->oLog->error(__FILE__.':'.__LINE__.':write file_exists');
			//���͗p��tmp�t�@�C�������݂��Ȃ���΋����I��
			return;
		}
		$fp = fopen($fnameCtoP,'w');
		if(!flock($fp,LOCK_EX)){
			//lock�����Ȃ���Ε\���Ȃ��ɂ���
			return;
		}
// 		$this->oLog->INFO(__FILE__.':'.__LINE__.':fopen');
		$wRes = fwrite($fp," ".PHP_EOL,2);
		foreach($this->screen->arrScreenStr as $key=>$view){
			//HTML�̎d�l�Ȃ͂�����FILE���������|���Ȃ��̂ōŏ��X�y�[�X
			$wRes = fwrite($fp," ".$view->strStartTag.PHP_EOL,strlen($view->strStartTag.PHP_EOL) + 1);
			//echo�\�����Ȃ���Β��g�̕\��������
			if(empty($view->echoElem)){
				if(count($view->arrLineElem) > 0){
					foreach($view->arrLineElem as $key=>$elem){
						//�s���폜�������ɍ폜����Ă���\�����肂
						if(isset($elem)){
							//������̕\��
							if($elem->type == 'TEX'){
								$wRes = fwrite($fp,$elem->getText().PHP_EOL);
							}
							//���͍��ڂ̕\��
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
					//�s���폜�������ɍ폜����Ă���\�����肂
					if(isset($elem)){
						//�r��(����)
						if($elem->type == 'UND'){
							$wRes = fwrite($fp,$elem->getText().PHP_EOL);
						}
						//�r��(��)
						if($elem->type == 'BOX'){
							$wRes = fwrite($fp,$elem->getText().PHP_EOL);
						}
						//�r��(�c��)
						if($elem->type == 'VER'){
							$wRes = fwrite($fp,$elem->getText().PHP_EOL);
						}
						//�r��(���)
						if($elem->type == 'OVE'){
							$wRes = fwrite($fp,$elem->getText().PHP_EOL);
						}
					}
				}
			}
		}
		if(count($this->screen->execErrorArray) > 0){
			$count = 0;
			//�G���[�������ϊ����ďo�͌`����
			$wRes = fwrite($fp,'<input type="hidden" class="error"  value="'. $this->screen->exchengeFormatStringFromArray() .'">');
		}
// 		$this->oLog->INFO(__FILE__.':'.__LINE__.':fclose');
		fclose($fp);
	}
}

?>

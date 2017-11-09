<?php
//画面の表示位置に対する属性を格納
//author koyama
//create 20141019
//対応属性:REV
class clsLineAttr{
	public $sCol;        //属性開始位置
	public $eCol;        //属性終了位置
	public $attrName;    //属性名(REV)
	
// 	function __construct(){
// 		//空で作られる場合何もしない
// 	}
	
	function __construct($startCol,$endCol,$attrName){
		$this->sCol     = $startCol;
		$this->eCol     = $endCol;
		$this->attrName = $attrName;
	}
}
?>
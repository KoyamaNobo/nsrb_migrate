<?php
//テスト作成 日進ゴム　ソース解析用
//author koyama
//create 2013.09.18

class clsMenuItem{
	public $title = ''; 		//画面に対するファイル名
	
	function __construct($title){
//		$this->title = mb_convert_encoding($title,'utf8','sjis');
		$this->title = $title;
	}
}
?>
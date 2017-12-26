<?php
//
// 画面位置行単位の設定
// author : koyama
// create : 20131225
class clsLine{
	public $strStartTag = '<div>';
	public $strEndTag   = '</div>';
	public $arrLineElem;  //添え字配列:添え字はカラムに対応
	public $echoElem    = '';       //shからのエコーで表示形に送るもの
	public $attr;                   //属性情報を格納 中にはclsLineAttrクラスを格納
	public $replaceFlg  =1;           //上書きモードの時1に(画面に値を反映させるかどうか)
	public $oLog;


	function __construct($numLine){
		require_once('./lib/log.php');
		require_once('./lib/constant.php');
		require_once('./lib/clsLineAttr.php');
		require_once('./lib/clsLineElem.php');
		$this->strStartTag = '<div class="line l'.$numLine.'">';
		$this->oLog = New Log('');

		$this->attr = array();
		$this->arrLineElem = array();
	}

	//echoでshから送られる文字情報
	function setEchoText($text){
		$text = str_replace(' ','&nbsp;',$text);
		$this->echoElem = '<span tabindex="1" class="">'.$text.'</span>';
	}

	function getEchoText(){
		if(!empty($this->echoEle)){
			return $this->echoElem;
		}else{
			return 0;
		}
	}

	//Attributeの必要なくなった部分を除去
	function resetAttr($num,$sCol,$eCol){
		//属性の先頭から文字がある
		if($this->attr[$num]->sCol >= $sCol){
			//終了も一緒
			if($this->attr[$num]->eCol == ($eCol + 1)){
				unset($this->attr[$num]);
			}else{
				$this->attr[$num]->sCol = $eCol + 1;
			}
		}else{
			if($this->attr[$num]->eCol <= $eCol){
				$this->attr[$num]->eCol = $sCol - 1;
			}else{
				//文字の後ろ側を作成
				$this->attr[] =  New clsLineAttr($eCol + 1, $this->attr[$num]->eCol,$this->attr[$num]->attrName);
				$this->attr[$num]->eCol = $sCol - 1;
			}
		}
	}

	//文字情報のセット(CON)
	function setColText($text,$line,$col){
		$col = (int)$col;
		$elemStrLen = 0;
		$dataFlg = false;
		//データ表示はflgを立てておく
		if(preg_match('/^\s*d.*"/',$text)){
			$text = substr($text,2);
			$dataFlg = true;
		}
		//"から"までを取り出す
		$text = substr($text,(strpos($text,'"')+1),( strrpos($text,'"') - (strpos($text,'"')+1) ));
		$elemStrLen = strlen($text);

		//スタートが重なる部分を削除
		foreach($this->arrLineElem as $key => $LineElem){
			$diff = 0;

			//なぜか$LineElemのほうが取れないのでkeyで判断
			if(!preg_match('/TEX$/',$this->arrLineElem[$key]->type)){
				continue;
			}

			//開始位置からなので終了は-1になる
			//スタートもエンドも一緒のものは削除して新規
			if(!is_null($this->arrLineElem[$key]) && $this->arrLineElem[$key]->sCol === $col && $this->arrLineElem[$key]->eCol ===(($col - 1) + $elemStrLen)){
				unset($this->arrLineElem[$key]);
				continue;
			}

			//中身がすべてスペースの時も中身は削除
			if(preg_match('/^\s+$/',$text) && $this->arrLineElem[$key]->sCol >= $col && $this->arrLineElem[$key]->eCol <= (($col - 1) + $elemStrLen) ){
				unset($this->arrLineElem[$key]);
				continue;
			}

			//文字が埋まっているもの
			//スペースの連続はそこに埋まっているものを消す
			//エンドが今までのスタートからエンドに含まれるものは重なる文字数分後ろを削る
			//部分を削る構造になっていない comment koyama
			//条件１(新)終了が(既)範囲に含まれる
			//条件２(新)開始が(既)範囲に含まれる
			if((($this->arrLineElem[$key]->sCol <= ($col + $elemStrLen) && $this->arrLineElem[$key]->eCol >= ($col + $elemStrLen) )
			|| ($this->arrLineElem[$key]->sCol <= $col && $this->arrLineElem[$key]->eCol >= $col)
			|| ($this->arrLineElem[$key]->sCol >= $col && $this->arrLineElem[$key]->eCol <= ($col + $elemStrLen)))){
				//差分の位置の計算
				//ここまで一緒
				if((int)$col > (int)$this->arrLineElem[$key]->sCol){
					$same = ((int)$col - (int)$this->arrLineElem[$key]->sCol);
				}else{
					//元あった文字列部分の前方には一致なし
					$same = 0;
				}
				//ここまで違う
				//飛び出したときはどうなるか確認
				if((int)$col > (int)$this->arrLineElem[$key]->sCol){
					$difflength = (int)($elemStrLen);
				}else{
					$difflength = ((int)($col + $elemStrLen) -(int)$this->arrLineElem[$key]->sCol);
				}
				//置換対象の文字列を作成
				$replacement = str_repeat(" ",$difflength);

				//属性があれば一致した部分の属性を削除
				$this->arrLineElem[$key]->unsetAttr((int)$col,(int)($col + $elemStrLen));
				//中の_HTML生成に使われるので先にセット
				$this->arrLineElem[$key]->reSetPartText($replacement,$same,$difflength);
			}
		}

		//開始位置からなので終了は-1になる
		if($dataFlg == true){
			$temp = New clsLineElem($line,$col,(($col - 1) + $elemStrLen),'TEX',$text,'','d');
		}else{
			$temp = New clsLineElem($line,$col,(($col - 1) + $elemStrLen),'TEX',$text,'','');
		}
		$this->arrLineElem[] = $temp;
// 		ksort($this->arrLineElem);
		//部分的なreverse
		//属性の追加
		if(count($this->attr)){
			foreach($this->attr as $num => $tmpAttr){
				if(isset($tmpAttr)){
					switch($tmpAttr->attrName){
					case PHP_CLASS_REVERSE:
						//事前に入力がなかったものはあとからかける
						//これはかけたら消す
						if($temp->setReverse((int)$tmpAttr->sCol,(int)$tmpAttr->eCol)){
							$this->resetAttr($num,(int)$col,(int)(($col - 1) + $elemStrLen));
						}
						break;
					case PHP_CLASS_BLINK:
						//事前に入力がなかったものはあとからかける
						//これはかけたら消す
						if($temp->setBlink((int)$tmpAttr->sCol,(int)$tmpAttr->eCol)){
							$this->resetAttr($num,(int)$tmpAttr->sCol,(int)$tmpAttr->eCol);
						}
						break;
					}
				}
			}
		}
	}

	//REVERSEの属性をHTMLに書き落とし
	function setReverse($line,$sCol,$eCol){
		$targText = "";
		$targKey  = "";
		$existFlg = false;   //範囲に入る要素が存在するかどうか

		foreach($this->arrLineElem as $targElem){
			if(isset($targElem) && preg_match('/TEX$/',$targElem->type)){
				//同じ行ならtargetに
				if($targElem->setReverse((int)$sCol,(int)$eCol)){
					//ターゲットに入ったものがあればtrueに
					$existFlg = true;
				}
			}
		}

		//範囲に入るものがあっても保存する必要ある？
		if($existFlg == false){
			//属性を保存
			$this->attr[] = New clsLineAttr($sCol, $eCol,PHP_CLASS_REVERSE);
		}
	}

	//BLINKの属性をHTMLに書き落とし
	function setBlink($line,$sCol,$eCol){
		$targText = "";
		$targKey  = "";
		$existFlg = false;   //範囲に入る要素が存在するかどうか
		//重ね合わせがあるだけで一つのはず
		foreach($this->arrLineElem as $targElem){
			if(isset($targElem) && strcmp($targElem->type ,'TEX') === 0){
				//同じ行ならtargetに
				if($targElem->setBlink((int)$sCol,(int)$eCol)){
					//ターゲットに入ったものがあればtrueに
					$existFlg = true;
				}
			}
		}

		//範囲に入るものがあっても保存する必要あり?
		if($existFlg == false){
			//属性を保存
			$this->attr[] = New clsLineAttr($sCol, $eCol,PHP_CLASS_BLINK);
		}
	}

	//下線のセット(UND)
	function setUnderLine($startline,$startcol,$endcol){
		$height = 0;
		$width = ($endcol - $startcol);
		$tmpText = '<div class="boxline'.((int)$startline + 1) .' f'.(int)$startcol.' box und" style="height:'.$height .'em; width:'.($width / 2).'em;"></div>';
		$this->arrLineElem[] = New clsLineElem($startline,$startcol,($startcol + $width),'UND',$tmpText,NULL,NULL);
	}

	//罫線(箱型)のセット(BOX)
	function setBox($startline,$startcol,$endline,$endcol){
		if($endcol === 0){
			$width = 0;
		} else {
			$width = $endcol - $startcol;
		}
		if($endline === 0){
			$height = 1;
		} else {
			$height = ($endline - $startline + 1) ;
		}
		$tmpText = '<div class="boxline'.(int)$startline .' f'.(int)$startcol.' box" style="height:'.$height . 'em; width:'.($width / 2).'em;"></div>';
		$this->arrLineElem[] = New clsLineElem($startline,$startcol,($startcol + $width),'BOX',$tmpText,NULL,NULL);
	}

	//罫線(上線)のセット(OVE)
	function setOverLine($startline,$startcol,$endcol){
		$height = 0;
// 		if($endline === 0){
// 			$width = 1;
// 		} else {
			$width = ($endcol - $startcol);
// 		}
		$tmpText = '<div class="boxline'.(int)$startline .' f'.(int)$startcol.' box ove" style="height:'.$height . 'em; width:'.($width / 2).'em;"></div>';
		$this->arrLineElem[] = New clsLineElem($startline,$startcol,($startcol + $width),'OVE',$tmpText,NULL,NULL);
	}

	//罫線(縦線)のセット(VER)
	function setVertical($startline,$startcol,$endline){
		$width = 0;
		if($endline === 0){
			$height = 1;
		} else {
			$height = ($endline - $startline + 1);
		}
		$tmpText = '<div class="boxline'.(int)$startline .' f'.(int)$startcol.' box ver" style="height:'.$height . 'em; width:'.($width / 2).'em;"></div>';
		$this->arrLineElem[] = New clsLineElem($startline,$startcol,($startcol + $width),'VER',$tmpText,NULL,NULL);
	}
	//BUZZERのセット
	function setBuzzer($startLine,$startColumn,$modalFlg,$soundsLength){
		$element  = '';
		if(preg_match('/B/',$modalFlg)){
			$element .= '<input id="err-buz" class="f'.($startColumn).'" type="hidden" name="err-buz" value="'.$soundsLength.'" />';
		} elseif(preg_match('/J/',$modalFlg)){
			$element .= '<input id="info-buz" class="f'.($startColumn).'" type="hidden" name="info-buz" value="'.$soundsLength.'" />';
		}

		$this->arrLineElem[] = New clsLineElem($startLine,$startColumn,($startColumn + 0),'INP',$element,NULL,NULL);
	}

	//入力項目のセット  (INP)
	//COBOLから
	function setInput($startcol,$endcol,$argname,$addclass){
		$startcol = (int)$startcol;
		$endcol   = (int)$endcol;
		$length   = $endcol - $startcol;
		$element  = '';
		$values   = '';

		//"から"までを取り出す
		$text = '';
		for($i = 0;$i < ($endcol - $startcol);$i++){
			$text .= '&nbsp;';
		}

		//TODO :20170330: VALUEを設定する必要があるかも
		//20170713 VALUEを探す処理追加
		if($this->replaceFlg == 1){
			foreach($this->arrLineElem as $tLineElem){
			//TEXT系以外のノードは関係なし
				if( !( $tLineElem->dataType == $tLineElem->arrKind[0] || $tLineElem->dataType == $tLineElem->arrKind[1] )){
				continue;
				}
				//一致するものがあればそれを代入
				if($startcol == $tLineElem->sCol && $endcol == ($tLineElem->eCol + 1)  ){
					$values = $tLineElem->origText;
					break;
				}
				//とりあえず、中で取得できるかどうか試してみる
				//上で値が入らなかったら要素を切らないといけない add koyama 20171010
				//今回指定のinputを包含していれば処理する
				//完全一致のものがあることを考えてbrakeしない
				if($startcol >= $tLineElem->sCol && $endcol <= ($tLineElem->eCol + 1)  ){
					$values = substr($tLineElem->origText,($startcol - $tLineElem->sCol),($endcol - $startcol));
					// $this->oLog->info("values         :".$values.__FILE__.':'.__LINE__);
				}
			}
		}
		// $this->oLog->info(":".$values.__FILE__.':'.__LINE__);
		//classを変更する必要がないので書き換え
		$element .= '<input class="nextinput f'.($startcol).' '.$addclass.'" type="text" name="'.$argname.'" maxlength="'.$length.'" size="'.$length.'" value="'.$values.'" style="width:'.($length / 2).'em;" />';

// 		$this->arrLineElem =  New clsLineElem($startColumn,($startcol + $length),'INP',$element);
		//行はいらないので0でセット
		// $this->oLog->info(":".$element.__FILE__.':'.__LINE__);
		$this->arrLineElem[] = New clsLineElem(0,$startcol,($startcol + $length),'INP',$element,NULL,NULL);
	}

	//inputを画面にひとつにする
	function remInput(){
// 		$this->oLog->info(.":"print_r($this->arrLineElem,true));
		foreach($this->arrLineElem as $key => $targElem){
			if(isset($targElem) && strcmp($targElem->type ,'INP') === 0){
				unset($this->arrLineElem[$key]);
			}
// 			unset($targElem['INP']);
		}
	}

}
?>

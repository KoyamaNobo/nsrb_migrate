<?php
//画面の表示位置に対する属性を格納
//author koyama
//create 20141019
//対応属性:REV,BLI
class clsLineElem{
	public $line;        //属性開始位置
	public $sCol;        //属性開始位置
	public $eCol;        //属性終了位置
	public $type;    //'TEX','dTEX','INP','VER','OVE','BOX','UND'
	public $dataType;    //'TEX','dTEX'の区別
	public $text;
	public $attr;    //属性名(REV,BLI)(classに直に埋められるときは埋めるため)
	public $partAttr;    //clsLineAttrで一部分に属性がつくものを制御(配列で複数指定可能にする)
	public $origText;
	public $arrKind;
	public $oLog;

// 	function __construct(){
// 		//空で作られる場合何もしない
// 	}
	function __construct($line,$startCol,$endCol,$type,$text,$attr,$dataFlg){
		require_once('./lib/log.php');
		require_once('./lib/constant.php');
		require_once('./lib/clsLineAttr.php');
		$this->arrKind = array('TEX','dTEX','INP','VER','OVE','BOX','UND');
		$this->line     = $line;
		$this->sCol     = $startCol;
		$this->eCol     = $endCol;
		$this->type     = $type;
		if(preg_match('/d/',$dataFlg)){
			$this->dataType = $this->arrKind[1];
		}elseif(preg_match('/s/',$dataFlg)){
			$this->dataType = $this->arrKind[0];
		}else{
			$this->dataType = '';
		}
		// 画面の同じ位置(行-列)に全角SPと半角SPが書き出される場合の文字化け対策として、全て半角SPに変換していたが、実際に書き出すタイミングで文字化け対策を行うように対応
		// (clsLineElem+reSetPartTextとclsLine+setInputのタイミング)
		$this->origText = $text;

		$this->attr     = $attr;
		$this->partAttr = Array();
		$this->oLog     = New Log('');

		//textは出力の直前に決めるので、ここでセットしない
		if(array_search($type,$this->arrKind) === FALSE ){
			//定義がなかった時
			$this->text     = $text;
		}else if(array_search($type,$this->arrKind) == 0){
			$this->reSetText($this->origText);
		}else if(array_search($type,$this->arrKind) == 1){
			$this->reSetText($this->origText);
		}else{
			//それ以外の指定子のとき
			$this->text     = $text;
		}
	}

	function getText(){
		//追加で属性指定がなければそのまま出力.あればそれを組み込んで出力
		if(count($this->partAttr) == 0){
			return $this->text;
		}else{
			//出力対象文字列を初期化
			$this->text = '';
			$loopCount = 0;
			//文字列全体を一旦格納
			$partText = $this->origText;
			//中の値を消すため移す
			$tempArray = $this->partAttr;
			usort($tempArray,"cmp_clsAttr");
			foreach($tempArray as $num => $partAttr){
				if($partAttr->sCol < $this->eCol && $partAttr->eCol < $this->sCol){
					//属性で分割等の結果関係なくなったものは飛ばす
					continue;
				}
				//ひとつ目の指定より前があったら
				if($loopCount === 0 && ($partAttr->sCol - $this->sCol) > 0){
					$partText = '';
					//全体の指定で
					$partText .='<span class="'.$this->attr.'l'.$this->line.' f'.$this->sCol.'">';
					//指定の前までを
					$partText .= preg_replace('/ /','&nbsp;',mb_strcut($this->origText,0,(($partAttr->sCol - $this->sCol)-1)));
					$partText .='</span>';
					$this->text .= $partText;
				}
// $this->oLog->info("::".$this->origText."::".$this->sCol.":".$this->eCol.":".print_r($partAttr,true).__FILE__.__LINE__);
				//指定有りの
				$partText = '';
				$startColumn = 0;
				//属性指定が要素より早い時のために
				if($partAttr->sCol < $this->sCol){
					$startColumn = $this->sCol;
				}else{
					$startColumn = $partAttr->sCol;
				}
				//全体の指定＋ a で
				if($partAttr->attrName == PHP_CLASS_REVERSE){
					//かぶるもの探す
					if(array_key_exists($num+1,$tempArray) && $tempArray[$num]->sCol == $tempArray[$num+1]->sCol){
						$partText .='<span class="' . ' ' .$this->attr.'l'.$this->line.' f'.$startColumn.'">';
// 						$partText .='<span class="'.HTML_CLASS_REVERSE. ' '. HTML_CLASS_BLINK. ' ' .$this->attr.'l'.$this->line.' f'.$startColumn.'">';
						//削除する代わり
						$tempArray[$num+1]->sCol = 0;
						$tempArray[$num+1]->eCol = 0;
					}else{
						$partText .='<span class="'.HTML_CLASS_REVERSE. ' ' .$this->attr.'l'.$this->line.' f'.$startColumn.'">';
					}
				}else{
					//かぶるもう一方を探す
					//存在すれば消す
					if(array_key_exists($num+1,$tempArray) && $tempArray[$num]->sCol == $tempArray[$num+1]->sCol){
						$partText .='<span class="' . ' ' .$this->attr.'l'.$this->line.' f'.$startColumn.'">';
						//削除する代わり
						$tempArray[$num+1]->sCol = 0;
						$tempArray[$num+1]->eCol = 0;
					}else{
						$partText .='<span class="'.HTML_CLASS_BLINK. ' ' .$this->attr.'l'.$this->line.' f'.$startColumn.'">';
					}
				}

				//開始位置(ブロック内での)
				$startPoint = ($partAttr->sCol - $this->sCol);
				//内側から文字が始まっている場合
				if($startPoint < 0){
					$startPoint = 0;
				}

				//長さ
				$length = $partAttr->eCol - ($partAttr->sCol - 1);

				//指定の前までを
				$partText .= preg_replace('/ /','&nbsp;',mb_strcut($this->origText,$startPoint,$length));
				$partText .='</span>';
				$this->text .= $partText;

				$loopCount++;

				//最後の要素で残りがある場合
				if(($loopCount) === count($this->partAttr) && (($partAttr->eCol - $this->eCol) !== 0)){
					$partText = '';
					//全体の指定で
					//指定の後ろを
					//開始位置(ブロック内での)
					//終了位置が飛び出ているケースは除去されている
					$startPoint = (($partAttr->eCol) - ($this->sCol-1));
					$partText .='<span class="'.$this->attr.'l'.$this->line.' f'. ($partAttr->sCol + $startPoint).'">';
					$partText .= preg_replace('/ /','&nbsp;',mb_strcut($this->origText,$startPoint,($this->eCol+1) - $partAttr->eCol));
					$partText .='</span>';
					$this->text .= $partText;
				}
			}
			//一度出力したら属性をすべて消す
			$this->partAttr = array();
			return $this->text;
		}
	}


	//origText入れなおし
	//return 元の中身がある true,元の中身がスペースのみ false;
	function reSetPartText($replacement,$start,$diff){
		//sameが-の場合は後ろから,+の場合は前から
		if(strlen(bin2hex(mb_strcut($this->origText,$start,$diff,CHARSET))) != strlen(bin2hex(substr($this->origText,$start,$diff))) ){
			// ファイルの文字コードからCHARSETの文字コードに変換して全角1文字のバイト数を取得
			$multibyteLength = strlen(mb_convert_encoding("あ", CHARSET, "UTF-8"));

			// 切り出し開始位置の文字がマルチバイト文字で、半バイト分だけ切り出しの対象になる場合
			if (mb_strcut($this->origText, $start, 1, CHARSET) == ""
					&& mb_strcut($this->origText, $start, $multibyteLength, CHARSET) != substr($this->origText, $start, $multibyteLength)) {
				// 切り出し位置をずらす。
				$start = $start - 1;
				$diff = $diff + 1;
			}

			// 切り出し終了位置の文字がマルチバイト文字で、半バイト分だけ切り出しの対象になる場合
			if (mb_strcut($this->origText, $start + $diff, 1, CHARSET) == ""
					&& mb_strcut($this->origText, $start + $diff, $multibyteLength, CHARSET) != substr($this->origText, $start + $diff, $multibyteLength)) {
				// 切り出し位置をずらす。
				$diff = $diff + 1;
			}

			//置換対象の文字列を作成
			$replacement = str_repeat(" ",$diff);
		}
		$this->origText = substr_replace($this->origText,$replacement,$start,$diff);
		//入出力があった部分は属性を解除
		$inStart = $start + $this->sCol;
		$inEnd   = $inStart + $diff;
		$this->reSetText($this->origText);
	}

	function unsetAttr($inStart,$inEnd){
		foreach($this->partAttr as $num => $partAttr){
			if(($inStart >=  $partAttr->sCol) && ($inStart >=  $partAttr->eCol)){
				//入力が含まれる属性
				if($inStart ===  $partAttr->sCol){
					//開始位置が同じ時は開始位置を入力の終了まで動かす
					$partAttr->sCol = ($inEnd +1);
				}elseif($inEnd ===  $partAttr->eCol){
					//終了位置が同じ時は開始位置を入力の開始まで動かす
					$partAttr->eCol = ($inStart - 1);
				}else{
					//開始終了共に違うときは
					//前側を先に作る
					$this->partAttr[] = New clsLineAttr($partAttr->sCol, ($inStart - 1),PHP_CLASS_BLINK);
					//開始位置を入力の終了まで動かす
					$partAttr->sCol = ($inEnd +1);
				}
				//修正分で順番が崩れるのでsort
				usort($this->partAttr,"cmp_clsAttr");
			}
		}
	}

	//文字列要素に対する部分的なReverse設定
	function setPartReverse($attrStart,$attrEnd){
		$existsFlg = false;
		//全てを範囲に入れるものがあればそれを一つ指定
		if($attrStart == $this->sCol && $attrEnd == $this->eCol ){
			$this->partAttr = array();
			//追加される要素を追加
			$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
			return;
		}
		foreach($this->partAttr as $num => $partAttr){
			//中に含まれるものがあれば分割する
			if($attrStart < $partAttr->sCol && $partAttr->attrName === PHP_CLASS_BLINK){
				//開始位置が早い
				if($attrEnd < $partAttr->eCol){
					//終了位置が早い
					//追加される要素より後ろがあるので追加
					$this->partAttr[] = New clsLineAttr(($attrEnd + 1), $partAttr->eCol,PHP_CLASS_BLINK);
					//追加される要素と同じ位置で重なったものを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//追加される要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//foreachで取り出したものを直前までに区切る
					$partAttr->eCol = ($partAttr->sCol - 1);
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrEnd == $partAttr->eCol){
					//終了位置が同じ
					//追加される要素と同じ位置で重なったものを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//追加される要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//foreachで取り出したものを直前までに区切る
					$partAttr->eCol = ($attrStart - 1);
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrStart < $partAttr->eCol){
					//終了位置が遅い
					//開始位置が前なら範囲が重なる
					//追加される要素と同じ位置で重なったものを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_BLINK);
					//追加される要素を重なったものと同じ終了までで追加
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_REVERSE);
					//追加される要素を重なったものと同じ終了からで追加
					$this->partAttr[] = New clsLineAttr(($partAttr->eCol + 1), $attrEnd,PHP_CLASS_REVERSE);
					//foreachで取り出したものを直前までに区切る
					$partAttr->eCol = ($partAttr->sCol - 1);
					//既に要素を追加したflg
					$existsFlg = true;
				}
			}elseif($attrStart == $partAttr->sCol){
				//開始位置が同じ
				//完全一致はそのまま追加
				if($attrEnd < $partAttr->eCol){
					//先にあるものが長い
					//追加される要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//追加される要素より後ろがあるので追加
					$this->partAttr[] = New clsLineAttr(($attrEnd + 1), $partAttr->eCol,PHP_CLASS_BLINK);
					//foreachで取り出したものを挿入分と同じ長さで
					$partAttr->eCol = $attrEnd;
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrEnd > $partAttr->eCol){
					//先にあるものが短い
					//追加される要素(先にあるものと同じ長さ)を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_REVERSE);
					//追加される要素(先にあるもの以降)を追加
					$this->partAttr[] = New clsLineAttr(($partAttr->eCol + 1), $partAttr->eCol,PHP_CLASS_REVERSE);
					//既に要素を追加したflg
					$existsFlg = true;
				}
			}else{
				//開始位置が遅い
				if($attrEnd < $partAttr->eCol){
					//終了位置が早い
					//追加される要素の後ろ側を追加
					$this->partAttr[] = New clsLineAttr(($attrEnd + 1), $partAttr->eCol,PHP_CLASS_BLINK);
					//追加される要素の前側側は短くして対応
					$partAttr->eCol = ($attrStart - 1);
					//追加される要素の後ろをを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//追加要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrEnd == $partAttr->eCol){
					//終了位置が同じ
					//追加される要素の前側は短くして対応
					$partAttr->eCol = ($attrStart - 1);
					//追加される要素をの後ろ側
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//追加要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrStart < $partAttr->eCol){
					//終了位置が遅い
					//開始位置が前なら範囲が重なる
					//追加される要素の後ろをを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_BLINK);
					//追加する要素の前側を
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_REVERSE);
					//foreachで取り出したものを挿入分と同じ長さで
					$partAttr->eCol = ($attrStart -1);
					//追加する要素の後ろを
					$this->partAttr[] = New clsLineAttr(($partAttr->eCol + 1), $attrEnd,PHP_CLASS_REVERSE);
					//既に要素を追加したflg
					$existsFlg = true;
				}
			}
		}
		//重なったものが存在しない
		if(!$existsFlg){
			$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
		}
		//追加分で順番が崩れるのでsort
		if(count($this->partAttr) > 0){
			usort($this->partAttr,"cmp_clsAttr");
		}
	}

	//初期定義用
	function reSetText($text){
		$this->origText = $text;
		//nbspに変換すると文字の長さが変わるためここで
		$text = str_replace(' ','&nbsp;',$text);
		$this->text     = '<span class="'.$this->attr .'l'.$this->line.' f'.$this->sCol.'">'.$text.'</span>';
	}

	//文字列要素に対するReverse設定
	//return 範囲に入るか 入る:true,入らない:false
	function setReverse($attrStart,$attrEnd){
		$retVal = false;
		//フルにReverseの時
		if( (int)$this->sCol >= (int)$attrStart && (int)$this->eCol <= (int)$attrEnd){
			//特例にしていたが他と同じ処理に
			//掛ける位置より要素の長さは小さいので
			$this->setPartReverse((int)$this->sCol,(int)$this->eCol);

			//対象が全て空白の時は当てていない扱いに
			if(strlen(trim($this->origText)) != 0){
				$retVal = true;
			}else{
				$retVal = false;
			}
		}elseif((int)$attrStart > (int)$this->sCol && (int)$attrEnd < (int)$this->eCol){
			//一部の時
			//両方共飛び出ているケース
			$start  = 0;
			$length = 0;
			//要素の長さより掛ける位置は小さいので
			$this->setPartReverse((int)$attrStart,(int)$attrEnd);
			$length = $this->eCol - $this->sCol;

			//対象が全て空白の時は当てていない扱いに
			if(strlen(trim(mb_strcut($this->origText,$start,$length))) != 0){
				$retVal = true;
			}else{
				$retVal = false;
			}
		}elseif((((int)$attrStart >= (int)$this->sCol) && ((int)$attrStart < (int)$this->eCol))
		|| ((((int)$attrEnd) > (int)$this->sCol) && (((int)$attrEnd) < (int)$this->eCol))){
			//一部の時
			$start  = 0;
			$length = 0;
			if((int)$this->sCol >= (int)$attrStart){
				//両方共中に入っているケースはうえで見ているので後ろが飛び出し
				$this->setPartReverse($this->sCol,$attrEnd);
				$start = $attrStart - $this->sCol;
				$length = $this->eCol - $attrStart;
			}else{
				//両方共中に入っているケースはうえで見ているので前が飛び出し
				$this->setPartReverse($attrStart,$attrEnd);
				$length = $this->eCol - $attrStart;
			}

			//対象が全て空白の時は当てていない扱いに
			if(strlen(trim(mb_strcut($this->origText,$start,$length))) != 0){
				$retVal = true;
			}else{
				$retVal = false;
			}
		}

// $this->oLog->info(":".$this->line.":".$this->origText.":".$this->sCol.":".$this->eCol.":".$attrStart."::".$attrEnd.":".__FILE__.__LINE__);
// $this->oLog->info("::::::".print_r($temp,true).":::::::".__FILE__.__LINE__);
// $this->oLog->info("::::::".(int)$retVal.":::::::".__FILE__.__LINE__);
		//このエレメントは行が一致しているだけ
		return $retVal;
	}

	//文字列要素に対する部分的なBlink設定
	function setPartBlink($attrStart,$attrEnd){
		$existsFlg = false;
		//全てを範囲に入れるものがあればそれを一つ指定
		if($attrStart == $this->sCol && $attrEnd == $this->eCol ){
			$this->partAttr = array();
			//追加される要素を追加
			$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
			return;
		}
		foreach($this->partAttr as $num => $partAttr){
			//中に含まれるものがあれば分割する
			if($attrStart < $partAttr->sCol && $partAttr->attrName === PHP_CLASS_REVERSE){
				if($attrEnd < $partAttr->eCol){
					//終了位置が早い
					//追加される要素より後ろがあるので追加
					$this->partAttr[] = New clsLineAttr(($attrEnd + 1), $partAttr->eCol,PHP_CLASS_BLINK);
					//追加される要素と同じ位置で重なったものを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//追加される要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//foreachで取り出したものを直前までに区切る
					$partAttr->eCol = ($partAttr->sCol - 1);
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrEnd == $partAttr->eCol){
					//終了位置が同じ
					//追加される要素と同じ位置で重なったものを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//追加される要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//foreachで取り出したものを直前までに区切る
					$partAttr->eCol = ($partAttr->sCol - 1);
					//既に要素を追加したflg
					$existsFlg = true;
				}else{
					//終了位置が遅い
					//追加される要素と同じ位置で重なったものを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_REVERSE);
					//追加される要素を重なったものと同じ終了までで追加
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_BLINK);
					//追加される要素を重なったものと同じ終了からで追加
					$this->partAttr[] = New clsLineAttr(($partAttr->eCol + 1), $attrEnd,PHP_CLASS_BLINK);
					//foreachで取り出したものを直前までに区切る
					$partAttr->eCol = ($partAttr->sCol - 1);
					//既に要素を追加したflg
					$existsFlg = true;
				}
			}elseif($attrStart == $partAttr->sCol){
				//開始位置が同じ
				if($attrEnd < $partAttr->eCol){
					//先にあるものが長い
					//追加される要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//追加される要素より後ろがあるので追加
					$this->partAttr[] = New clsLineAttr(($attrEnd + 1), $partAttr->eCol,PHP_CLASS_REVERSE);
					//foreachで取り出したものを挿入分と同じ長さで
					$partAttr->eCol = $attrEnd;
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrEnd > $partAttr->eCol){
					//先にあるものが短い
					//追加される要素(先にあるものと同じ長さ)を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_BLINK);
					//追加される要素(先にあるもの以降)を追加
					$this->partAttr[] = New clsLineAttr(($partAttr->eCol + 1), $partAttr->eCol,PHP_CLASS_BLINK);
					//既に要素を追加したflg
					$existsFlg = true;
				}
			}else{
				//開始位置が遅い
				if($attrEnd < $partAttr->eCol){
					//終了位置が早い
					//追加される要素の後ろ側を追加
					$this->partAttr[] = New clsLineAttr(($attrEnd + 1), $partAttr->eCol,PHP_CLASS_REVERSE);
					//追加される要素の前側側は短くして対応
					$partAttr->eCol = ($attrStart - 1);
					//追加される要素の後ろをを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//追加要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrEnd == $partAttr->eCol){
					//終了位置が同じ
					//追加される要素の前側は短くして対応
					$partAttr->eCol = ($attrStart - 1);
					//追加される要素をの後ろ側
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_REVERSE);
					//追加要素を追加
					$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
					//既に要素を追加したflg
					$existsFlg = true;
				}elseif($attrStart < $partAttr->eCol){
					//終了位置が遅い
					//開始位置が前なら範囲が重なる
					//追加される要素の後ろをを追加
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_REVERSE);
					//追加する要素の前側を
					$this->partAttr[] = New clsLineAttr($attrStart, $partAttr->eCol,PHP_CLASS_BLINK);
					//foreachで取り出したものを挿入分と同じ長さで
					$partAttr->eCol = ($attrStart -1);
					//追加する要素の後ろを
					$this->partAttr[] = New clsLineAttr(($partAttr->eCol + 1), $attrEnd,PHP_CLASS_BLINK);
					//既に要素を追加したflg
					$existsFlg = true;
				}
			}
		}
		//重なったものが存在しない
		if(!$existsFlg){
			$this->partAttr[] = New clsLineAttr($attrStart, $attrEnd,PHP_CLASS_BLINK);
		}
		//追加分で順番が崩れるのでsort
		usort($this->partAttr,"cmp_clsAttr");
	}

	//文字列要素に対するBlink設定
	function setBlink($attrStart,$attrEnd){
		$retVal = false;
		//フルにblinkの時
		if( (int)$this->sCol >= (int)$attrStart && (int)$this->eCol <= (int)$attrEnd){
			//特例にしていたが他と同じ処理に
			//掛ける位置より要素の長さは小さいので
			$this->setPartBlink((int)$this->sCol,(int)$this->eCol);

			//対象が全て空白の時は当てていない扱いに
			if(strlen(trim($this->origText)) != 0){
				$retVal = true;
			}else{
				$retVal = false;
			}
		}elseif((int)$attrStart > (int)$this->sCol && (int)$attrEnd <= (int)$this->eCol){
			//両方共飛び出ているケース
			$this->setPartBlink((int)$this->sCol,(int)$this->eCol);

			//対象が全て空白の時は当てていない扱いに
			if(strlen(trim($this->origText)) != 0){
				$retVal = true;
			}else{
				$retVal = false;
			}
		}elseif((((int)$attrStart >= (int)$this->sCol) && ((int)$attrStart <= (int)$this->eCol))
		|| ((((int)$attrEnd) >= (int)$this->sCol) && (((int)$attrEnd) <= (int)$this->eCol))){
			//一部の時
			if((int)$this->sCol >= (int)$attrStart){
				//両方共中に入っているケースはうえで見ているので後ろが飛び出し
				$this->setPartBlink((int)$attrStart,(int)$this->eCol);
			}else{
				//両方共中に入っているケースはうえで見ているので前が飛び出し
				$this->setPartBlink((int)$this->sCol,(int)$attrEnd);
			}

			//対象が全て空白の時は当てていない扱いに
			if(strlen(trim(mb_strcut($this->origText,$start,$length))) != 0){
				$retVal = true;
			}else{
				$retVal = false;
			}
		}
		//このエレメントは行が一致しているだけ
		return $retVal;
	}
}
function cmp_clsAttr( $a , $b){
	if($a->sCol < $b->sCol){
		$cmp = -1;
	}elseif($a->sCol > $b->sCol){
		$cmp = 1;
	}else{
		$cmp = 0;
	}
	if( $cmp == 0 ){
		if($a->eCol < $b->eCol){
			$cmp = -1;
		}elseif($a->eCol > $b->eCol){
			$cmp = 1;
		}else{
			$cmp = 0;
		}
		if($cmp == 0){
			$cmp = strcmp( $a->attrName , $b->attrName ) ; //属性名を比較
		}
	}

	return $cmp;
}
?>

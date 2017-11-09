<!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/dataExchange.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>MAP DataExchange</title>
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
<script type="text/javascript" src="./js/common.js"></script>
<script type="text/javascript" src="./js/backButton.js?ver=20160613"></script>
<script type="text/javascript" src="./js/menuShortcut.js?ver=20160613"></script>
</head>
<body>
	<div class="contentbody">
		<div class="hblock">
			<div class="lhBlock">
				<h1><?php
				if(!isset($oConf)||empty($oConf->getintExtState())){
					echo "ファイルアップローダー";
				}else{
					echo "一時ファイルエクスプローラー";
				}?></h1>
			</div>
		</div>
		<div class="mhblock">
			<div class="lmhblock">
			</div>
			<div class="rfblock">
				<div class="lineLeft">
					<span>アップロード<span>
					<form action="?sort=<?php echo $sort; ?>" method="post" enctype="multipart/form-data">
						<input name="userfile[]" type="file" value="アップロード開始" multiple />
						<input class="button" type="submit" value="アップロード開始" />
					</form>
					<div class="massage">
						<span style="color:red;font-size:50%;">20ファイルを超えたものは受信されません</span>
<?php
if(isset($messages) && !empty($messages)){
	foreach ($messages as $value){
		echo "$value";
	}
}
?>
					</div>
				</div>
				<input class="button backButton" type="button" value="戻る">
			</div>
		</div>
		<div class="mfblock">
			一覧
			<div class="lmfblock">
				<div class="table">
					<table>
						<tr class="header">
							<td  class="filename" >
								ファイル名
								<a href="?sort=nameDESC" class="sort">▼</a>
								<a href="?sort=nameASC" class="sort">▲</a>
							</td>
							<td>
								ファイルサイズ
								<a href="?sort=sizeDESC" class="sort">▼</a>
								<a href="?sort=sizeASC" class="sort">▲</a>
							</td>
							<td>
								最終更新日時
								<a href="?sort=lastmodDESC" class="sort">▼</a>
								<a href="?sort=lastmodASC" class="sort">▲</a>
							</td>
						</tr>
<?php
if(isset($files) && !empty($files)){
	foreach ($files as $value){
?>
						<tr>
							<td>
								<a href="<?php if(isset($nextExeName) && !empty($nextExeName)){echo $nextExeName;}else{ echo './DataExchangeDetail.php';} ?>?filename=<?php echo urlencode(mb_convert_encoding($value["name"],"cp932", "utf8"));?> ">
									<?php echo mb_convert_encoding($value["name"],"cp932", "utf8");?>
								</a>
							</td>
							<td class="right"><?php echo  number_format($value['filesize']);?>&nbsp;Byte</td>
							<td class="right"><?php echo $value['lastmod'];?></td>
						</tr>
<?php
	}
}?>
					</table>
				</div>
			</div>
		</div>
		<div class="fblock">
			<div class="lmhblock">
			</div>
			<div class="rfblock">
				<input class="button backButton" type="button" value="戻る">
			</div>
		</div>
	</div>
</body>
</html>

<!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/dataExchange.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>MAP DataExchangeDetail</title>
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
<script type="text/javascript" src="./js/common.js"></script>
<script type="text/javascript" src="./js/menuShortcut.js?ver=20160613"></script>
<script type="text/javascript" src="./js/backButton.js?ver=20160613"></script>
<script type="text/javascript" src="./js/submitButton.js?ver=20160613"></script>
</head>
<body>
	<div class="contentbody">
		<div class="hblock">
			<div class="lhBlock">
				<h1>ファイル詳細</h1>
			</div>
		</div>
		<div class="mblock">
			<div class="lmfblock">
				<form action="#" method="post" name="frm">
					<div class="table">
						<table>
							<tr class="header">
								<td class="filename2">ファイル名</td>
								<td>ファイルサイズ</td>
								<td>最終更新日</td>
								<td>&nbsp;</td>
							</tr>
							<tr>
								<td>
									<input class="filename" type="text" name="filename" value="<?php echo htmlspecialchars($file['name'],ENT_QUOTES,'SJIS');?>">
									<input type="hidden" name="filename_befor" value="<?php echo htmlspecialchars($file['name_befor'],ENT_QUOTES,'SJIS');?>">
								</td>
								<td class="right"><?php echo number_format($file['filesize']);?>&nbsp;Byte</td>
								<td class="right"><?php echo ($file['lastmod']);?>
								</td>
								<td>
									<input class="button submit" id="update" name="update" type="button" value="更新">
									<input class="button submit" id="download" name="download" type="button" value="ダウンロード">
									<input class="button submit" id="remove" name="remove" type="button" value="削除">
								</td>
							</tr>
						</table>
					</div>
					<input type="hidden" id="action" name="action" value="">
				</form>
			</div>
		</div>
		<div class="massage">
<?php
if(isset($messages) && !empty($messages)){
	foreach ($messages as $value){
		echo "$value";
	}
}
?>
		</div>
		<div class="fblock">
			<div class="lfblock">
				<input class="button backButton" type="button" value="戻る">
			</div>
		</div>
	</div>
</body>
</html>

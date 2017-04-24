<?php
//header("Content-type: text/html; charset=SJIS");
//header("Cache-Control: Private");
?><!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/DataView.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>DataView</title>
<script type="text/javascript" src="../js/jquery-1.10.2.min.js"></script>
<script type="text/javascript" src="./js/DataView_common.js?var=20160708"></script>
<script type="text/javascript" src="./js/DataView_UserConf.js?var=20160708"></script>
</head>
<body>
	<div class="contentbody">
		<div class="hblock">
			DataView
		</div>
		<div class="mblock">
<?php
require_once('./view/vw_Part_DataView_LoginInfo.php');
require_once('./view/vw_Part_DataView_ConstMessage.php');
require_once('./view/vw_Part_DataView_Menu.php');
?>

			<div class="main">
				<div id="navigate" class="navigate">
					<span class="bold">ユーザ設定</span>
					<div class="description">
						ユーザの追加・削除を行ってください。<br/>
						編集は選択行更新より行えます。
					</div>
				</div>
				<div id="successMessage" class="successMessage"><?php echoMessages($successCode) ?></div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				</div>
			</div>
			<div class="contents">
				<div id="rcontents" class="rcontents userList">
					<span>ユーザ一覧<span>
					<div id="rightListHeader" class="rightListHeader">
						<div class="listHead">
							<span class="User_Id">ユーザID</span><span
							class="Authority_Flg">権限</span><span
							class="Disp_Num">表示件数</span><span
							class="Del_Flg">削除</span>
						</div>
					</div>
					<div id="userListBody" class="leftList">
<?php
$oddFlg = -1;
if(!empty($users)){
	foreach($users as $user){
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span><span
									class="User_Id bgChItems"><?php echo htmlEscape($user['User_Id']); ?></span><span
									class="Authority_Flg bgChItems"><?php echo htmlEscape($user['A_Flg_Disp']); ?></span><span
									class="Disp_Num right bgChItems"><?php echo htmlEscape($user['Disp_Num']); ?></span><span
									class="Del_Flg bgChItems"><?php echo htmlEscape($user['Del_Flg_Jpn']); ?></span><span
									class="hidden userChildLast bgChItems id"><?php echo htmlEscape($user['User_Id']); ?></span>
								</div>
<?php
	$oddFlg *= -1;
	}
}
?>
					</div>
				</div>
				<div>
					<img src="./img/btntbn06-1.png" alt="追加" id="addButton" class="modalOpenButton modalAdd" width="" height="">
					<img src="./img/btntbn06-3.png" alt="更新" class="modalOpenButton modalUpdate" width="" height="">
					<img src="./img/btntbn06-2.png" alt="選択行を削除" id="delButton"  class="modal_delOpenButton" width="" height="">
				</div>
			</div>
		</div>
	</div>
	<div id="modal">
		<div id="modalBody" class="userSetting">
			<div id="modalHeader">&nbsp;</div>
					<form id="inputForm" class="form_input">
						<div class="form_elem">
							<span class="form_label">ユーザID</span><span class="form_sep">:</span>
							<input type="text" id="User_Id" name="User_Id" value="<?php $User_Id ?>">
						</div>
						<div class="form_elem">
							<span class="form_label">パスワード</span><span class="form_sep">:</span>
							<input type="password" id="User_Password" name="User_Password" value="<?php echo $User_Password ?>">
						</div>
						<div class="form_elem">
							<span class="form_label">パスワード確認用</span><span class="form_sep">:</span>
							<input type="password" id="User_Conf" name="User_Conf" value="<?php echo $User_Password ?>">
						</div>
						<div class="form_elem">
							<span class="form_label">表示件数</span><span class="form_sep">:</span>
							<input type="text" id="Disp_Num" name="Disp_Num" value="<?php echo $Disp_Num ?>">
						</div>
						<div class="form_elem">
							<span class="form_label">権限</span><span class="form_sep">:</span>
							<input type="radio" id="userAuthority_none" name="userAuthority" value="0" checked="checked">
							<label>なし</label>
							<input type="radio" id="userAuthority_yes" name="userAuthority" value="1">
							<label>あり</label>
						</div>
					</form>
			<div  id="modalButton">
				<p class="modalCloseButton center">閉じる</p>
				<p class="modeButton center">更新</p>
				<div id="errMessageModal" class="errMessageModal"></div>
			</div>
		</div>
	</div>
	<div id="modal_del">
		<div id="modal_delBody" class="userSetting">
			<div id="modal_delHeader">&nbsp;</div>
				<div class="del_message">
					<span class="del_message">ID:「」をデータベースより削除します。よろしいですか？</span>
				</div>
			<div  id="modalButton">
				<p class="modalCloseButton center">閉じる</p>
				<p class="delButton center">削除</p>
				<div id="errMessageModal" class="errMessageModal"></div>
			</div>
		</div>
	</div>
</body>
</html>

<?php 
//header("Content-type: text/html; charset=SJIS");
//header("Cache-Control: Private"); 
?>
<!DOCTYPE html>
<head>
<meta charset="SJIS">
<link rel="stylesheet" href="./css/styleReset.css" type="text/css">
<link href="./css/DataView.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="/icons/favicon.ico" />
<title>DataView</title>
<script type="text/javascript" src="./js/DataView_common.js"></script>
<script type="text/javascript" src="./js/jquery-1.10.2.min.js"></script>
<script type="text/javascript" src="./js/DataView_UserConf.js"></script>

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
			shell_exec('logger -i " ~~ '.print_r($Disp_Num,true).'"');
?>

			<div class="main">
				<div id="navigate" class="navigate">
					<span class="bold">���[�U�ݒ�</span>
					<div class="description">
						���[�U�̍X�V���s���Ă��������B<br/>
						�����[�UID�͕ҏW�ł��܂���B
					</div>
				</div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<div class="transition">
					<p class="backButton center">�߂�</p>
					<p class="updateButton center">�X�V</p>
				</div>
			</div>
			<div class="contents">
				<form id="inputForm" class="inputForm">
					<div class="form_elem">
						<span class="form_label">���[�UID</span><span class="form_sep">:</span>
						<input type="text" id="User_Id" name="User_Id" class="gray" readonly="readonly" value="<?php echo $User_Id; ?>">
					</div>
					<div class="form_elem">
						<span class="form_label">�p�X���[�h</span><span class="form_sep">:</span>
						<input type="password" id="User_Password" name="User_Password" value="<?php echo $User_Password; ?>">
					</div>
					<div class="form_elem">
						<span class="form_label">�p�X���[�h�m�F�p</span><span class="form_sep">:</span>
						<input type="password" id="User_Conf" name="User_Conf" value="<?php echo $User_Conf; ?>">
					</div>
					<div class="form_elem">
						<span class="form_label">�\������</span><span class="form_sep">:</span>
						<input type="text" id="Disp_Num" name="Disp_Num" value="<?php echo $Disp_Num; ?>">
					</div>
<?php 
$checked ='';
$none    ='';
if($_SESSION['dv_user_authority_flg'] == "1"){
	if( $Authority_Flg == "1"){ 
		$checked = 'checked="checked"';
	}else{ 
		$none = 'checked="checked"';
	}
?>
					<div class="form_elem">
						<span class="form_label">����</span><span class="form_sep">:</span>
						<input type="radio" id="userAuthority_none" name="userAuthority" value="0" <?php echo $none; ?>>
						<label>�Ȃ�</label>
						<input type="radio" id="userAuthority_yes" name="userAuthority" value="1" <?php echo $checked; ?>>
						<label>����</label>
					</div>
<?php 
}
?>
				</form>
			</div>
			<div class="transition">
				<p class="backButton center">�߂�</p>
				<p class="updateButton center">�X�V</p>
			</div>
		</div>
	</div>
</body>
</html>
			<div class="box">
				<div class="menu">
					<ul type="disc">
<?php
$url = basename($_SERVER['PHP_SELF'],".php");
if (preg_match("/DataView_index(.)*/", $url)) {
	echo '<li name="DataView_menu" class="menuLink DataView_menu selected"><label>�g�b�v</label></li>';
}else if(preg_match("/DataView_menu(.)*/", $url)){
	echo '<li name="DataView_menu" class="menuLink DataView_menu selected"><label>�g�b�v</label></li>';
}else{
	echo '<li name="DataView_menu" class="menuLink DataView_menu"><label>�g�b�v</label></li>';
}

if (preg_match("/DataView_command_search(.)*/", $url)) {
	echo '<li name="DataView_command_search" class="menuLink DataView_command_search selected"><label>�����^�O����</label></li>';
}else if (preg_match("/DataView_disp(.)*/", $url) && preg_match("/DataView_command_search(.)*/", $prePgName)) {
	echo '<li name="DataView_command_search" class="menuLink DataView_command_search selected"><label>�����^�O����</label></li>';
}else{
	echo '<li name="DataView_command_search" class="menuLink DataView_command_search"><label>�����^�O����</label></li>';
}

if (preg_match("/DataView_free_search(.)*/", $url)) {
	echo '<li name="DataView_free_search_1"  class="menuLink DataView_free_search_1 selected"><label>���R����</label></li>';
}else if (preg_match("/DataView_disp(.)*/", $url) && preg_match("/DataView_free_search(.)*/", $prePgName)) {
	echo '<li name="DataView_free_search_1" class="menuLink DataView_free_search_1 selected"><label>���R����</label></li>';
}else{
	echo '<li name="DataView_free_search_1" class="menuLink DataView_free_search_1"><label>���R����</label></li>';
}

if (preg_match("/DataView_item_conf(.)*/", $url)) {
	echo '<li name="DataView_item_conf" class="menuLink DataView_item_conf selected"><label>�a���ݒ�</label></li>';
}else{
	echo '<li name="DataView_item_conf" class="menuLink DataView_item_conf"><label>�a���ݒ�</label></li>';
}

if (preg_match("/users_item(.)*/", $url)) {
	echo '<li name="users_item" class="menuLink users_item selected"><label>���ڐݒ�</label></li>';
}else{
	echo '<li name="users_item" class="menuLink users_item"><label>���ڐݒ�</label></li>';
}

if (preg_match("/DataView_user_conf(.)*/", $url)) {
	echo '<li name="DataView_user_conf" class="menuLink DataView_user_conf selected"><label>���[�U�ݒ�</label></li>';
}else{
	echo '<li name="DataView_user_conf" class="menuLink DataView_user_conf"><label>���[�U�ݒ�</label></li>';
}

echo '<li name="DataView_logout" class="menuLink DataView_logout"><LABEL>���O�A�E�g</LABEL></li>';
?>

					</ul>
				</div>
			</div>

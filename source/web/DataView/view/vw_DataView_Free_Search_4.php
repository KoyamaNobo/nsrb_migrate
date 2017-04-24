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
<script type="text/javascript" src="../js/jquery-1.12.4.min.js"></script>
<script type="text/javascript" src="./js/DataView_common.js?var=20160613"></script>
<script type="text/javascript" src="./js/DataView_free_search_4.js?var=20160613"></script>
<script type="text/javascript" src="./js/DataView_backButton.js?var=20160613"></script>
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
				<div class="navigate">
					<span class="bold">���R����</span>
					<div class="description">
						���o���������ݒ肵�Ă��������B<br/>
						�e�[�u���I�������������ݒ聄�\�����ڑI����<span class="red">���o�����ݒ�</span>�����בւ��ݒ聄���ʕ\��
					</div>
				</div>
				<div class="summary">

<?php
require_once('./view/vw_Part_DataView_summaryTables.php');
require_once('./view/vw_Part_DataView_summaryBonds.php');
require_once('./view/vw_Part_DataView_summaryItemNames.php');
?>

				</div>
				<div id="errMessage" class="errMessage"><?php echoMessages($message_codes) ?></div>
				<div class="transition">
					<p class="backButton DataView_free_search_3 center">�߂�</p>
					<p class="filterButton center">���o�����ǉ�</p>
					<p class="nextButton center">����</p>
				</div>
				<div class="contents">
					<div id="rcontents" class="rcontents Wide62">
						���o�����ꗗ
						<div id="filterListHeader"  class="filterListHeader">
							<div class="listHead">
								<span class="listAndOr">AND/OR</span><span
								class="listTableName">�e�[�u����</span><span
								class="listItemName">���ږ�</span><span
								class="listJapaneseName">�a��</span><span
								class="listOperator">��r���Z�q</span><span
								class="listValue">�l</span>
							</div>
						</div>
						<div>
							<div id="filterList"  class="filterList">
<?php
$oddFlg = -1;
if(!empty($selectedFilters)){
	$filterCount = 0;
	foreach($selectedFilters as $selectedFilter){
	$tmpJN="";
	if($selectedFilter['JapaneseName'] == " "){
		$tmpJN = "";
	}else{
		$tmpJN = $selectedFilter['JapaneseName'];
	}
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<span class="hidden bgChItems"></span><span
									class="listAndOr bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;�T�C�Y �F <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;�^ �F <?php echo htmlEscape($selectedFilter['type']); ?>"><?php if($filterCount != 0){ echo htmlEscape($selectedFilter['andOr']);}else{echo "&nbsp;";} ?></span><span
									class="listTableName bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;�T�C�Y �F <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;�^ �F <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['tableName']); ?></span><span
									class="listItemName bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;�T�C�Y �F <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;�^ �F <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['itemName']); ?></span><span
									class="listJapaneseName bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;�T�C�Y �F <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;�^ �F <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['JapaneseName']); ?></span><span
									class="hidden listS_point bgChItems"><?php echo htmlEscape($selectedFilter['s_point']); ?></span><span
									class="hidden listSize bgChItems"><?php echo htmlEscape($selectedFilter['size']); ?></span><span
									class="hidden listType bgChItems"><?php echo htmlEscape($selectedFilter['type']); ?></span><span
									class="listOperator bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;�T�C�Y �F <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;�^ �F <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['operator']); ?></span><span
									class="listValue bgChItems" title="�J�n�ʒu : <?php echo htmlEscape($selectedFilter['s_point']); ?>&#x0A;�T�C�Y �F <?php echo htmlEscape($selectedFilter['size']); ?>&#x0A;�^ �F <?php echo htmlEscape($selectedFilter['type']); ?>"><?php echo htmlEscape($selectedFilter['value']); ?></span><span
									class="hidden bgChItems id"><?php echo htmlEscape($selectedFilter['andOr']."~".$selectedFilter['tableName']."~".$selectedFilter['itemName']."~".$tmpJN."~".$selectedFilter['s_point']."~".$selectedFilter['size']."~".$selectedFilter['type']."~".$selectedFilter['operator']."~".$selectedFilter['value']."~".$selectedFilter['id']); ?></span>
								</div>
<?php
	$oddFlg *= -1;
	$filterCount++;
	}
}
?>
							</div>
						</div>
						<img src="./img/btntbn06-2.png" id="filterDelButton" alt="�ナ�X�g�I���s���폜" width="" height="">
					</div>
				</div>
				<div class="transition">
					<p class="backButton DataView_free_search_3 center">�߂�</p>
					<p class="filterButton center">���o�����ǉ�</p>
					<p class="nextButton center">����</p>
				</div>
			</div>
		</div>
	</div>
</body>
</html>

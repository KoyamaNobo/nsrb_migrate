<?php 
$oddFlg = -1;
if(!empty($selectedItemNames)){
	foreach($selectedItemNames as $selectedItemName){ 
?>
								<div class="listElem <?php if($oddFlg == 1){ echo ' odd'; } ?>">
									<?php outputLeftList($selectedItemName,$listTypeFlg) ?>
								</div>
<?php
	$oddFlg *= -1;
	}
}
?>
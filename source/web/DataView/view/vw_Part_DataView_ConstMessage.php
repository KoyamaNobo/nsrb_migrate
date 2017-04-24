			<div class="ConstMessage">
<?php
foreach ($MESSAGE as $key => $value){
 ?>
					<input type="hidden" id="<?php echo $key; ?>" name="<?php echo $key; ?>" value="<?php echo $value; ?>">
<?php
}
 ?>

			</div>
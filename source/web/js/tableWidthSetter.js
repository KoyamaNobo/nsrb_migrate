var tableWidthSetter = function () {

	var headerColsbg_color = $('.tableHeader .tableR .bg_color');
	var headerColsfont_color = $('.tableHeader .tableR .font_color');
	var headerColsfont_size = $('.tableHeader .tableR .font_size');
	var headerColshi_fontcolor = $('.tableHeader .tableR .hi_fontcolor');
/*
	$('.tableData .tableR .bg_color').width($(headerColsbg_color).width());
	$('.tableData .tableR .font_color').width($(headerColsfont_color).width());
	$('.tableData .tableR .font_size').width($(headerColsfont_size).width());
	$('.tableData .tableR .hi_fontcolor').width($(headerColshi_fontcolor).width());
*/

	$('.tableData .tableR .bg_color').css( 'width',$(headerColsbg_color).width() + 'px');
	$('.tableData .tableR .font_color').css('width' ,$(headerColsfont_color).width() + 'px');
	$('.tableData .tableR .font_size').css( 'width',$(headerColsfont_size).width() + 'px');
	$('.tableData .tableR .hi_fontcolor').css( 'width' ,$(headerColshi_fontcolor).width() + 'px');

};
addEvent('load',window,tableWidthSetter);

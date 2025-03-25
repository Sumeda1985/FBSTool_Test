
Shiny.addCustomMessageHandler('set_active_tab', function(message) {
  
	setTimeout(function(){
	  $('.sidebar-menu > li > a').eq(message[0]).click()
	}, 200);
	
	
	setTimeout(function(){
	    console.log('.treeview-menu > li > a[href="' + message[1] + '"')
        $('.treeview-menu > li > a[href="' + message[1] + '"').click();
    }, 800);
    
   
});

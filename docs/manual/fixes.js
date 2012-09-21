$(document).ready(function(){
  $("dt+dt").each(function(){
    var b = this.previousSibling;
    
    $(b).css("display", "block");
  });


  $("dt+dd").each(function(){
    var dt = this.previousSibling;
    var dd = this;

    if ($(dt).width() <= 70)
      {
	$(dd)
	    .css("position", "relative")
	    .css("top", "-1.2em");
      }
    else
      {
	$(dd)
	    .css("margin-bottom", "15px");
      }
  });
});


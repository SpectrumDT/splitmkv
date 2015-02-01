/*jslint browser:true */
var SplitMKV = {
  onLoad: function () {
    // Set event listeners. 
    $("#button_extract").on("click", function(){SplitMKV.extract(this);});
    $("#input_xmlfile").on("change", function(){SplitMKV.getChaptersFromXml();});
    $("li").on("mouseover", function () {this.style.fontStyle = 'italic';});
    $("li").on("mouseout", function () {this.style.fontStyle = 'normal';});
    $("input").on("change", SplitMKV.setInputCookie);
    
    // Make sections slide in.
    //$("section.slidedown").hide().slideDown("slow");
    
    // Load texts from cookies.
    SplitMKV.getSavedTexts();
    
    // Load chapters from XML, if an XML file is preselected. 
    SplitMKV.getChaptersFromXml();
  },
  
  setInputCookie: function(input){    
    if (this.id){
      SplitMKV.Cookie.setCookie(this.id, this.value);
    }
  },
  
  getSavedTexts: function () {    
    var inputs, input, i, cookieValue;
    if (document.cookie){
      $("input[type='text']").each(function(){
        if (this.id){
          cookieValue = SplitMKV.Cookie.getCookie(this.id);
          if (cookieValue){
            $(this).val(cookieValue);
          }
        }
      });
    }
  },
  
  getChaptersFromXml: function (){
    var file = SplitMKV.getSelectedXmlFileName();
    if (file){
      SplitMKV.readXml(file);
    }
  },
  
  getSelectedXmlFileName: function(){
    var files;
    // Check for the various File API support.
    if (window.File && window.FileReader && window.FileList && window.Blob) {
      files = $("#input_xmlfile")[0].files; 
      if (files.length > 0){
        return files[0];
      }
    }
    return null;
  },
  
  readXml: function(filePath){
    var fr;
    try{
      fr = new FileReader();
      fr.onload = function(){SplitMKV.showChapterTable(this.result);};
      fr.readAsText(filePath);
    } catch(e) {
      alert("Could not read file '"+filePath+"'.\n"+e);
    }
  },
  
  showChapterTable: function(xml){
    //alert("Received file: \n"+xml); 
    $("#chapterTable_pleaseSelect").hide();
    $("#chapterTable").fadeIn(1000);
  },
  
  extract: function (selfDom){
    var delay = 1000;
    var self = $(selfDom)
    var oldText = self.text();
    self.text("Extracting...").prop('disabled',true);
    window.setTimeout(function () {
      self.text(oldText).prop('disabled',false);
    }, delay);
  },
  
  Cookie: {  
    setCookie: function (name, value, expires){
      if (name){
        if (expires){
          document.cookie = name + "=" + value + "; expires=" + expires;
        } else {
          document.cookie = name + "=" + value;
        }
      }
    }, 
    
    getCookie: function (name){
      var i, pair, cookies, cookieString;
      cookieString = document.cookie;
      if (cookieString){
        cookies = cookieString.split(";");
        for (i = 0; i < cookies.length; i+= 1){
          pair = cookies[i].split("=");
          if (pair[0].trim() === name){
            return pair[1];
          }
        }
      }
      return null;
    }
  }
};

//SplitMKV.onLoad();
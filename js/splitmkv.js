/*jslint browser:true */
var SplitMKV = {
  onLoad: function () {
    // Set event listeners. 
    $("#button_extract").on("click", function(){SplitMKV.extract(this);});
    $("#input_xmlfile").on("change", function(){SplitMKV.getChaptersFromXml();});
    $("li").on("mouseover", function () {this.style.fontStyle = 'italic';});
    $("li").on("mouseout", function () {this.style.fontStyle = 'normal';});
    $("input").on("change", SplitMKV.setInputCookie);
    $("img").on("mouseover", function () {$(this).fadeTo("fast", 0.1)})
            .on("mouseout", function () {$(this).fadeTo("fast", 5.0)});
    
    // Make sections slide in.
    $("section.slidedown").hide().slideDown("slow");
    
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
      fr.onload = function(){
        var helper = new SplitMKV.tableHelper(this.result);
        helper.show();
      };
      fr.readAsText(filePath);
    } catch(e) {
      alert("Could not read file '"+filePath+"'.\n"+e);
    }
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
  }, 
  
  tableHelper: function (xml){
    
    this.table = $("#chapterTable");
    this.xml = xml;
  
    this.show = function(){
      try{      
        // Remove all existing rows except the header. 
        this.table.find("tr:has(td)").remove();
      
        this.populate();
        $("#chapterTable_label").hide();
        this.table.fadeIn(1000);
      } catch (e) {
        $("#chapterTable_label").text(e);
        $("#chapterTable_label").fadeIn(1000);
        this.table.hide();
      }
    };
    
    this.populate = function(){
      var chapters, chapter, table, rawChapters, parsedXml, 
          i = 1,
          chapters = [];
      table = this.table;
      parsedXml = this.parse();
      parsedXml.find("ChapterAtom").each(function (){
        chapter = new SplitMKV.Chapter(this);
        table.append('<tr><td>'+ SplitMKV.Temp.pad2(i) +
          '</td><td><input type="text" value="'+chapter.name+'"/></td><td>'+chapter.formattedLength+'</td></tr>');
        i++;
      });
    };
    
    this.parse = function(){
      try {
        return $($.parseXML(this.xml));
      } catch (e) {
        throw ("Bad XML file.");
      }
    }
  },
  
  Temp: {
    makeText: function (len)
    {
      var text = "";
      var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

      for( var i=0; i < len; i++ )
          text += possible.charAt(Math.floor(Math.random() * possible.length));

      return text;
    },
    makeLength: function(){
      return (Math.random()*10).toFixed(0) + ":" + SplitMKV.Temp.pad2((Math.random()*60).toFixed(0));
    },    
    pad: function (n, width, z){
      z = z || '0';
      n = n + '';
      return n.length >= width ? n : new Array(width - n.length + 1).join(z) + n;
    },
    pad2: function (n){
      return SplitMKV.Temp.pad(n, 2, '0');
    }
  }
};
  
SplitMKV.Chapter = function(xml){
  var self = $(xml);
  this.uid = self.find("ChapterUID").text();
  this.start = moment(self.find("ChapterTimeStart").text(), "HH:mm:ss.SSS");
  this.end = moment(self.find("ChapterTimeEnd").text(), "HH:mm:ss.SSS");
  this.name = self.find("ChapterDisplay ChapterString").text();;  
  this.length = this.end.diff(this.start);
  this.formattedLength = moment(this.length).format("mm:ss.SSS");
};

SplitMKV.Chapter.prototype.toString = function(){
  return this.uid + " (" + this.start + " - " + this.end + ") ["+this.length+"]";
};

//SplitMKV.onLoad();
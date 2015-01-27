/*jslint browser:true */
var SplitMKV = {
  onLoad: function () {    
    SplitMKV.setListListeners();
    SplitMKV.getSavedTexts();
    SplitMKV.setInputListeners();
  },
  
  setListListeners: function () {
      $("li").on("mouseover", function () {this.style.fontStyle = 'italic';});
      $("li").on("mouseout", function () {this.style.fontStyle = 'normal';});
  },
  
  getSavedTexts: function () {    
    var inputs, input, i, cookieValue;
    if (document.cookie){
      //window.alert(document.cookie);
      inputs = document.getElementsByTagName("input");
      for (i = 0; i < inputs.length; i+= 1){
        input = inputs[i];
        if (input.id){
          cookieValue = SplitMKV.getCookie(input.id);
          if (cookieValue){
            input.value = cookieValue;
          }
        }
      }
    }
  },
  
  setInputListeners: function () {
    var i;
    var inputs = document.getElementsByTagName("input");
    var addListener = function (input){
        if (input.id){
          input.addEventListener("change", function () {
            SplitMKV.setCookie(input.id, input.value);
          });
        }        
      };
    for (i = 0; i < inputs.length; i+= 1){
      addListener(inputs[i]);
    }      
  },
  
  xmlFile_onChange: function (input){
    var files;
    // Check for the various File API support.
    if (window.File && window.FileReader && window.FileList && window.Blob) {
      files = input.files; 
      if (files.length > 0){
        window.alert("Selected XML file: " + files[0].name);
      }
    } else {
      window.alert('The File APIs are not fully supported in this browser.');
    }
  },
  
  extract: function (self){
    var delay = 1000;
    self.innerHTML = "Extracting...";
    self.disabled = true;
    window.setTimeout(function () {
      self.innerHTML = "Extract";
      self.disabled = false;      
    }, delay);
  },
  
  setCookie: function (name, value, expires){
    if (expires){
      document.cookie = name + "=" + value + "; expires=" + expires;
    } else {
      document.cookie = name + "=" + value;
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
};

//SplitMKV.onLoad();
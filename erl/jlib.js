function mfa_rpc(cmd, callback){
    var xhr = new XMLHttpRequest();
    xhr.open("GET",cmd,true);    
    xhr.send();
    xhr.onreadystatechange = function(){
	if(xhr.readyState == 4 && xhr.status == 200)
	{
	    callback(xhr.responseText);
	}
    }
}

function json_rpc(mod, func, args, callback){
    var xhr = new XMLHttpRequest();
    xhr.open("POST","jsoncgi",true);    
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.send(JSON.stringify({mod: mod, func:func, args:args}));

    xhr.onreadystatechange = function(){
	if(xhr.readyState == 4 && xhr.status == 200)
	{
	    callback(xhr.responseText);
	}
    }
}

function json_rpc_load(M,F,A){
    var f = function(response){
	var obj = JSON.parse(response);
	load_regions(obj);
    };
    json_rpc(M, F, A, f);
}

function load_regions(obj){
    for(var key in obj){
	if(obj.hasOwnProperty(key)) {
	    document.getElementById(key).innerHTML = obj[key];
	}
    }
}

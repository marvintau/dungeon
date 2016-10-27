
var cast_edit = new Vue({
 	el: '#cast-table',
 	data: { casts:[] },

 	created: function(){
 		self = this;

		$.get("/get_casts", function(data){
			self.casts = data;
		}).fail(function(error){
			self.casts = [{name: error}]
		});

 	}

});
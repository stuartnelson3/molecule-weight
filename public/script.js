$(document).on("click", "button", function(e) {
  var data = {
    peptide_sequence: $("input[name=peptide_sequence]").val(),
    weight: parseFloat($("input[name=weight]").val())
  };

  params = {
    type: 'POST',
    data: data,
    dataType: 'text',
    url: '/possible-matches',
    success: function(data) {
      $(".result").html(data)
    }
  }

  $.ajax(params);
});

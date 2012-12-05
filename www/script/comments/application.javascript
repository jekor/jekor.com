(function ($) {
  function POST(url, data) {
    return $.ajax(url, {'type': 'POST', 'contentType': 'application/json', 'data': JSON.stringify(data)});
  }

  function renderComments(comments) {
    return $($.map(comments, function (c) {return renderComment(c)[0];}));
  }

  function renderComment(comment) {
    var template = $('<div class="comment">'
                     + '<div class="comment-box">'
                       + '<div class="breadcrumb">'
                         + '<span class="date"></span>'
                       + '</div>'
                       + '<div class="body"></div>'
                     + '</div>'
                   + '</div>');
    template.attr('path', comment['path']);
    var slashes = comment['path'].match(/\//g);
    var depth = 0;
    if (slashes !== null) {
      depth = slashes.length;
    }
    template.css('margin-left', depth * 10 + '%');
    template.prepend($('<a><img class="avatar" src="http://www.gravatar.com/avatar/' + comment['emailhash'] + '.jpg?s=60"></a>'));
    $('.date', template).append(new Date(comment['time'] * 1000).toLocaleString());
    $('.body', template).text(comment['body']);
    if (comment['name'] !== null) {
      $('.breadcrumb', template).prepend($('<span class="signature"></span>').text(comment['name']));
    }
    if (comment['url'] !== null) {
      $('.signature', template).wrap($('<a></a>').attr('href', comment['url']));
    }
    $('.signature', template).text(comment['name']);
    $('<a class="btn btn-small">Reply</a>').click(function () {
      template.after(renderCommentBox('/' + $(this).closest('.comment').attr('path')));
    }).appendTo($('.body', template));
    return template;
  }

  function renderCommentBox(resourcePath) {
    var box = $('<form class="well">'
                + '<input name="name" placeholder="Name (optional)" class="span3"><br>'
                + '<input name="email" placeholder="Email (optional)" type="email" class="span3"><br>'
                + '<input name="url" placeholder="URL (optional)" type="url" class="span3"><br>'
                + '<textarea name="body" placeholder="Comment" required class="span3"></textarea><br>'
                + '<input type="submit" class="btn btn-primary" value="Submit"></input>'
              + '</form>');
    var slashes = resourcePath.match(/\//g);
    var depth = 0;
    if (slashes !== null) {
      depth += slashes.length;
    }
    box.css('margin-left', depth * 10 + '%');
    box.submit(function (e) {
      e.preventDefault();
      var inputs = ['name', 'email', 'url', 'body'];
      var data = {};
      for (var k in inputs) {
        var val = $.trim($('[name=' + inputs[k] + ']').val());
        if (val === '') {
          val = null;
        }
        data[inputs[k]] = val;
      }
      POST(window.location.pathname + '/comment' + resourcePath, data)
        .done(function () {
          // TODO: Format the comment and add a success box.
          window.location.reload();
        })
        .fail(function () {
          $('<br><br><div class="alert alert-error">'
            + '<button class="close" data-dismiss="alert">×</button>'
            + '<strong>Error!</strong> Failed to post comment.'
          + '</div>').appendTo(box);
        });
      return false;
    });
    return box;
  }

  $(function () {
    // Load comments on this article.
    $.get(window.location.pathname + '/comments')
     .done(function (comments) {
       $('#comments').append('<h2>Comments</h2>').append(renderComments(comments)).append(renderCommentBox(''));
     });
  });
})(jQuery);

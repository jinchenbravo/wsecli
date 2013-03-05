-module(my1st_SUITE).
-compile(export_all).

      all() ->
          [mod_exists].

      mod_exists(_) ->
          {module,wsecli_message} = code:load_file(wsecli_message).
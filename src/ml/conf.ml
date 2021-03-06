open Geneweb

let conf =
  let utm = Unix.time () in
  let tm = Unix.localtime utm in
  let open Adef in
  let open Config in
  { from = ""
  ; manitou = false
  ; supervisor = false
  ; wizard = false
  ; is_printed_by_template = false
  ; friend = false
  ; just_friend_wizard = false
  ; user = ""
  ; username = ""
  ; auth_scheme = NoAuth
  ; pure_xhtml = false
  ; command = ""
  ; indep_command = ""
  ; highlight = ""
  ; lang = ""
  ; default_lang = ""
  ; default_sosa_ref = (Gwdb.dummy_iper, None)
  ; multi_parents = false
  ; can_send_image = false
  ; authorized_wizards_notes = false
  ; public_if_titles = false
  ; public_if_no_date = false
  ; cancel_links = false
  ; setup_link = false
  ; access_by_key = false
  ; private_years = 0
  ; hide_names = false
  ; use_restrict = false
  ; no_image = false
  ; no_note = false
  ; bname = ""
  ; cgi_passwd = ""
  ; env = []
  ; senv = []
  ; henv = []
  ; base_env = []
  ; allowed_titles = lazy []
  ; denied_titles = lazy []
  ; xhs = ""
  ; request = []
  ; lexicon =  Hashtbl.create 0
  ; charset = ""
  ; is_rtl = false
  ; left = ""
  ; right = ""
  ; auth_file = ""
  ; border = 0
  ; n_connect = None
  ; today = { day = tm.Unix.tm_mday
            ; month = succ tm.Unix.tm_mon
            ; year = tm.Unix.tm_year + 1900
            ; prec = Sure
            ; delta = 0 }
  ; today_wd = tm.Unix.tm_wday
  ; time = (tm.Unix.tm_hour, tm.Unix.tm_min, tm.Unix.tm_sec)
  ; ctime = utm
  ; image_prefix = ""
  ; b_arg_for_basename = false
  }

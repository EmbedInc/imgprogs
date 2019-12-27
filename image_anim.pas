{   Program IMAGE_ANIM [<options>]
*
*   Build an animated image from mulitple still images.
}
program image_anim;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  n_cmds = 13;                         {number of possible commands}
  cmd_len_max = 7;                     {size of largest command name}
  max_msg_args = 5;                    {max arguments we can pass to a message}

  cmd_len_alloc = cmd_len_max + 1;     {chars to allocate for each command}
  cmds_len = cmd_len_alloc * n_cmds;   {total number of chars in commands list}

type
  cmd_t =                              {one command name}
    array[1..cmd_len_alloc] of char;

  cmds_t = record                      {array of all the command names}
    max: string_index_t;               {simulate a var string}
    len: string_index_t;
    str: array[1..n_cmds] of cmd_t;
    end;

  inimg_p_t = ^inimg_t;
  inimg_t = record                     {info about one input image}
    next_p: inimg_p_t;                 {pointer to info about next input image}
    name_p: string_var_p_t;            {pointer to image file name}
    sec: real;                         {seconds to display this image in animation}
    end;

var
  fnam_out:                            {output file name}
    %include '(cog)lib/string_treename.ins.pas';
  oname_set: boolean;                  {TRUE if the output file name already set}
  loop: boolean;                       {TRUE if animation is an infinite loop}
  waitf_set, waitl_set: boolean;       {WAITF and WAITL explicitly set}
  alpha: boolean;                      {TRUE if write alpha component to output}
  infirst_p: inimg_p_t;                {pointer to first input image info}
  inlast_p: inimg_p_t;                 {pointer to last input image info}
  in_p: inimg_p_t;                     {pointer to info for curr input image}
  nin: sys_int_machine_t;              {number of input frames in list}
  nframe: sys_int_machine_t;           {current 1-N frame number}
  mem_p: util_mem_context_p_t;         {pointer to private mem context}
  conn: file_conn_t;                   {scratch file I/O connection info}
  imgo: img_conn_t;                    {connection to output image file}
  img: img_conn_t;                     {scratch image connection}
  x_size, y_size: sys_int_machine_t;   {input image size in pixels}
  aspect: real;                        {whole image aspect ratio}
  rate: real;                          {animation frame rate}
  waitf, waitl: real;                  {display times for first/last images}
  zoom: sys_int_machine_t;             {pixel replication scale factor}
  x, y: sys_int_machine_t;             {current pixel coordinate}
  xo: sys_int_machine_t;               {output pixel X coordinate}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  comm: string_list_t;                 {image comments list}
  uform:                               {driver format string supplied by user}
    %include '(cog)lib/string8192.ins.pas';
  str:                                 {scratch string}
    %include '(cog)lib/string8192.ins.pas';
  rsuff: string;                       {file name suffixes for readable image types}
  ftype: string;                       {output image file type name}
  p: string_index_t;                   {scratch parse index}
  scan_p: img_scan1_arg_p_t;           {pointer to buffer for one scan line}
  scano_p: img_scan1_arg_p_t;          {pointer to output scan line buffer}

  opt:                                 {upcased command line option}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

  cmds: cmds_t := [                    {all the command names}
    max := cmds_len, len := cmds_len, str := [
      '-OUT   ',                       {1}
      '-IN    ',                       {2}
      '-INLIST',                       {3}
      '-STDIN ',                       {4}
      '-LOOP  ',                       {5}
      '-NLOOP ',                       {6}
      '-RATE  ',                       {7}
      '-WAITF ',                       {8}
      '-WAITL ',                       {9}
      '-FORM  ',                       {10}
      '-ZOOM  ',                       {11}
      '-ALPHA ',                       {12}
      '-NALPHA',                       {13}
      ]
    ];

label
  next_opt, err_parm, parm_bad, done_opts;
{
******************************************************************
*
*   Subroutine GET_SUFFIXES (RW, SUFF)
*
*   Return the list of image file name suffixes for the read/write mode(s)
*   indicated by RW.
}
procedure get_suffixes (               {get image file name suffixes}
  in      rw: file_rw_t;               {required image read/write capabilities}
  out     suff: string);               {list of suffixes separated by spaces}
  val_param;

var
  tylist: string_var256_t;             {list of image file type names}
  ty: string_var32_t;                  {one image file type name}
  sulist: string_var80_t;              {list of image file name suffixes}
  p: string_index_t;                   {parse index}
  stat: sys_err_t;                     {completion status code}

label
  loop_type, done_types;

begin
  tylist.max := sizeof(tylist.str);    {init local var strings}
  ty.max := sizeof(ty.str);
  sulist.max := sizeof(sulist.str);

  img_list_types (rw, tylist);         {get image file types for this access mode}
  p := 1;                              {init TYLIST parse index}
  sulist.len := 0;                     {init suffixes list to empty}

loop_type:                             {back here to extract each new type from list}
  string_token (tylist, p, ty, stat);  {extract next image file type name from list}
  if sys_error(stat) then goto done_types; {done getting image file type names ?}
  if sulist.len > 0 then begin         {appending to previous entries ?}
    string_append1 (sulist, ' ');
    end;
  string_append1 (sulist, '.');        {write first char in file name suffix}
  string_append (sulist, ty);          {add on rest of this file name suffix}
  goto loop_type;                      {back to get next image type name from list}
done_types:                            {all file type suffixes created}
{
*   SULIST contains a list of all the valid image file type file name suffixes.
}
  string_fill (sulist);                {make Pascal STRING in STR field}
  suff := sulist.str;                  {pass back suffixes list}
  end;
{
******************************************************************
*
*   Subroutine IN_IMAGE (FNAM)
*
*   Add the image file name in FNAM to the end of the list of input
*   frames.
}
procedure in_image (                   {add input image to list}
  in      fnam: string_treename_t);    {image file name to add to end of list}
  val_param;

var
  p: inimg_p_t;                        {pointer to input image descriptor}

begin
  if fnam.len <= 0 then return;        {no file name present ?}

  util_mem_grab (sizeof(p^), mem_p^, false, p); {allocate mem for descriptor}
  string_alloc (fnam.len, mem_p^, false, p^.name_p); {allocate string for file name}

  p^.next_p := nil;                    {this will be last name in list}
  string_copy (fnam, p^.name_p^);      {set file name}
  p^.sec := 1.0 / rate;                {set animation duration for this frame}

  if inlast_p = nil
    then begin                         {this is first chain entry}
      infirst_p := p;                  {set pointer to start of chain}
      end
    else begin                         {there are previous chain entries}
      inlast_p^.next_p := p;           {link to previous chain entry}
      end
    ;
  inlast_p := p;                       {update pointer to end of chain}

  nin := nin + 1;                      {count one more input image in the list}
  end;
{
******************************************************************
*
*   Subroutine EXPAND_PATT (PATT)
*
*   Expand the image input filenames pattern PATT and add each resulting
*   input image to the list.
}
procedure expand_patt (                {expand input images pattern}
  in      patt: string_treename_t);    {pattern to expand}
  val_param;

type
  ent_p_t = ^ent_t;
  ent_t = record                       {info about one matching directory entry}
    next_p: ent_p_t;                   {pointer to next entry in chain}
    n: sys_int_machine_t;              {sequence number matching "#"}
    name_p: string_var_p_t;            {pointer to leafname string}
    end;

  listp_t = array[1..1] of ent_p_t;    {array with pointer to each list entry}
  listp_p_t = ^listp_t;

var
  name, tnam: string_treename_t;       {scratch pathnames}
  dir: string_treename_t;              {directory name}
  lpat: string_leafname_t;             {pattern leafname}
  gnam: string_treename_t;             {image file generic leafname, scratch name}
  tk: string_var32_t;                  {scratch string for number conversion}
  mem_p: util_mem_context_p_t;         {private mem context for matching files list}
  list_p: ent_p_t;                     {pnt to first entry in matching files list}
  listp_p: listp_p_t;                  {pointer to array of list entry pointers}
  nlist: sys_int_machine_t;            {number of entris in matching files list}
  ent_p: ent_p_t;                      {current files list entry pointer}
  i, j: sys_int_machine_t;             {scratch integer and loop counter}
  ig: sys_int_machine_t;               {GNAM string index}
  in1, in2: sys_int_machine_t;         {number char index start/end in GNAM}
  seq: sys_int_machine_t;              {sequence number matching "#" in pattern}
  seq_set: boolean;                    {SEQ already set for this file name}
  conn: file_conn_t;                   {connection for reading directory}
  finfo: file_info_t;                  {info about a particular directory entry}
  patt_suff: boolean;                  {TRUE if pattern contains explicit suffix}
  stat: sys_err_t;                     {completion status code}

label
  found_num, loop, done_dir, done_list;

begin
  name.max := sizeof(name.str);        {init local var strings}
  tnam.max := sizeof(tnam.str);
  dir.max := sizeof(dir.str);
  lpat.max := sizeof(lpat.str);
  gnam.max := sizeof(gnam.str);
  tk.max := sizeof(tk.str);

  string_treename (patt, name);
  string_pathname_split (              {make pattern directory and leaf names}
   name,                               {input full pathname}
   dir,                                {directory containing path}
   lpat);                              {file name within directory}
  if lpat.len <= 0 then return;

  for i := 1 to lpat.len do begin      {scan all chars in pattern leaf name}
    if lpat.str[i] = '#' then goto found_num; {pattern contains seq num wildcard ?}
    end;
  in_image (patt);                     {no wildcard to expand, use pattern directly}
  return;
found_num:                             {pattern contains wildcard}

  string_fnam_unextend (lpat, rsuff, name); {try to remove image file type suffix}
  patt_suff := name.len < lpat.len;    {TRUE if LPAT ends with explicit suffix}

  file_open_read_dir (dir, conn, stat); {set up for reading directory contents}
  if sys_error(stat) then return;      {punt on any error}

  util_mem_context_get (               {create temp mem context for files list}
    util_top_mem_context, mem_p);
  mem_p^.pool_size := 32768;
  mem_p^.max_pool_chunk := 2048;
  list_p := nil;                       {init files list to empty}
  nlist := 0;

loop:                                  {back here to read each new directory entry}
  file_read_dir (                      {get next directory entry}
    conn,                              {connection to directory}
    [],                                {we don't need any info beyond entry name}
    name,                              {returned directory entry name}
    finfo,                             {additional info about this directory entry}
    stat);
  if sys_error(stat) then goto done_dir; {punt on end of directory or hard error}

  string_pathname_join (dir, name, gnam); {make complete pathname for this entry}
  string_treename (gnam, tnam);        {follow all links and expand this name}
  file_info (                          {get info about this candidate file name}
    tnam,                              {file inquiring about}
    [file_iflag_type_k],               {we want file type}
    finfo,                             {returned file info}
    stat);
  if sys_error(stat) then goto loop;   {punt this name on any error}
  if finfo.ftype <> file_type_data_k   {ignore this file if not ordinary data file}
    then goto loop;

  string_fnam_unextend (               {check for image file name suffix}
    name,                              {name that might end in suffix}
    rsuff,                             {all the suffixes of readable image files}
    gnam);                             {name with suffix removed}
  if gnam.len >= name.len then goto loop; {this is not a readable image file ?}
{
*   The directory entry in NAME represents a readable image file.  GNAM
*   is NAME with the image file type suffix stripped.
}
  if patt_suff then begin              {pattern has explicit file type suffix ?}
    string_copy (name, gnam);          {match against full file name}
    end;

  seq_set := false;                    {init to no sequence number extracted yet}
  ig := 1;                             {init GNAM string index}

  for i := 1 to lpat.len do begin      {once for each character in pattern}
    if lpat.str[i] <> '#' then begin   {no wildcard char in pattern here ?}
      if gnam.str[ig] <> lpat.str[i]   {definite mismatch with pattern ?}
        then goto loop;                {punt this directory entry, on to next}
      ig := ig + 1;                    {advance to next GNAM char}
      next;                            {back to check next character position}
      end;
    {
    *   The pattern contains the "#" wildcard character at this position.
    }
    if seq_set                         {previous sequence number already extracted ?}
      then goto loop;                  {we don't handle multiple # wildcards}
    in1 := ig;                         {save starting GNAM index of number, if any}
    while                              {loop over 0-9 digits in GNAM at this pos}
        (gnam.str[ig] >= '0') and
        (gnam.str[ig] <= '9')
        do begin
      ig := ig + 1;                    {this GNAM char is digit, advance to next}
      end;
    in2 := ig - 1;                     {save ending GNAM index of number, if any}
    if in2 < in1                       {no number matched "#" in pattern ?}
      then goto loop;                  {punt this directory entry, on to next}
    {
    *   The # in the pattern at I matches a number in GNAM from IN1 to IN2.
    *   IG has been updated to the next character after the number.
    }
    string_substr (gnam, in1, in2, tk); {extract number string into TK}
    string_t_int (tk, seq, stat);      {convert to integer in SEQ}
    if sys_error(stat) then goto loop; {punt this file name on error}
    end;                               {back to match against next pattern char}
{
*   The directory entry in NAME matches the pattern.  SEQ is the sequence
*   number matching the "#" in the pattern.
}
  util_mem_grab (                      {allocate memory for this new list entry}
    sizeof(ent_p^),                    {amount of memory to allocate}
    mem_p^,                            {parent memory context}
    false,                             {will not be individually deallocated}
    ent_p);                            {returned pointer to the new memory}
  string_alloc (name.len, mem_p^, false, ent_p^.name_p); {alloc mem for name string}

  ent_p^.n := seq;                     {set sequence number of this entry}
  string_copy (name, ent_p^.name_p^);  {set leafname for this entry}
  ent_p^.next_p := list_p;             {add new entry to start of chain}
  list_p := ent_p;
  nlist := nlist + 1;                  {count one more entry in matching files list}
  goto loop;                           {back to process next directory entry}
{
*   All done reading and testing directory entries.  This is the common
*   exit point once the directory has been successfully opened.
}
done_dir:
  file_close (conn);                   {close connection to directory}
  if nlist <= 0 then goto done_list;   {no file names matched the pattern}
{
*   Sort the list of matching entries by ascending sequence number.
}
  util_mem_grab (                      {allocate memory for the pointers array}
    sizeof(listp_p^[1]) * nlist,       {amount of memory to allocate}
    mem_p^,                            {parent memory context}
    false,                             {will not be individually deallocated}
    listp_p);                          {returned pointer to new memory}

  ent_p := list_p;                     {init pointer to first entry in list}
  for i := 1 to nlist do begin         {once for each list entry}
    listp_p^[i] := ent_p;              {fill in this pointer array entry}
    ent_p := ent_p^.next_p;            {pointer to next entry in the files list}
    end;

  for i := 1 to nlist-1 do begin       {outer sort loop}
    for j := i+1 to nlist do begin     {inner sort loop}
      if                               {entries are out of order ?}
          (listp_p^[j]^.n < listp_p^[i]^.n) or {seq number out of order ?}
          ( (listp_p^[j]^.n = listp_p^[i]^.n) and {same seq but name out of order ?}
            (string_compare(listp_p^[j]^.name_p^, listp_p^[i]^.name_p^) < 0))
          then begin
        ent_p := listp_p^[i];          {flip order of these two list entries}
        listp_p^[i] := listp_p^[j];
        listp_p^[j] := ent_p;
        end;
      end;
    end;
{
*   This array at LISTP_P points to the list entries in ascending order.
}
  for i := 1 to nlist do begin         {once for each list entry}
    string_pathname_join (dir, listp_p^[i]^.name_p^, tnam); {make full pathname}
    in_image (tnam);                   {add this pathname to animation frames list}
    end;                               {back for next matching files list entry}

done_list:                             {all done with the matching file names list}
  util_mem_context_del (mem_p);        {deallocate all memory used for list}
  end;
{
******************************************************************
*
*   Subroutine EXPAND_LIST (CONN, STAT)
*
*   Expand each name from the input images list file open on CONN.
}
procedure expand_list (                {read input images list and expand names}
  in out  conn: file_conn_t;           {connection to input images list stream}
  out     stat: sys_err_t);            {completion status code}
  val_param;

const
  max_msg_args = 1;                    {max arguments we can pass to a message}

var
  buf: string_var8192_t;               {one line input buffer}
  fnam: string_treename_t;             {file name from list}
  p: string_index_t;                   {parse index}
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;

label
  loop;

begin
  buf.max := sizeof(buf.str);          {init local var strings}
  fnam.max := sizeof(fnam.str);

loop:                                  {back here each new line from input stream}
  file_read_text (conn, buf, stat);    {read next line from input stream}
  if file_eof(stat) then return;       {hit end of input stream ?}
  string_unpad (buf);                  {delete trailing blanks from input line}
  if buf.len <= 0 then goto loop;      {ignore blank lines}
  p := 1;                              {init input line parse index}
  string_token (buf, p, fnam, stat);   {extract file name from input line}
  if sys_error(stat) then return;
  expand_patt (fnam);                  {expand this file name pattern}
  string_token (buf, p, fnam, stat);   {try to get another token from input line}
  if not string_eos(stat) then begin   {didn't hit end of line as expected ?}
    if sys_error(stat) then return;    {hard error ?}
    string_upcase (fnam);
    fnam.len := min(fnam.len, 20);
    sys_msg_parm_vstr (msg_parm[1], fnam);
    sys_message_bomb ('img', 'image_anim_lline_extra', msg_parm, 1);
    end;
  goto loop;                           {back to do next input line}
  end;
{
******************************************************************
*
*   Subroutine MAKE_FTYPE (FNAM, FTYPE)
*
*   Set FTYPE to the image file type indicated or defaulted from FNAM.
*   The GIF file type is used unless FNAM explicitly ends in a recognized
*   image file type suffix.
}
procedure make_ftype (                 {set image file type from image file name}
  in      fnam: string_treename_t;     {image file name, may contain explicit suffix}
  out     ftype: string);              {returned image file type string}
  val_param;

var
  suff: string;                        {list of image file type suffixes}
  name: string_treename_t;             {scratch pathname}

begin
  name.max := sizeof(name.str);        {init local var string}

  get_suffixes ([file_rw_write_k], suff); {get suffixes for writable image files}
  string_fnam_unextend (fnam, suff, name); {try to remove image type suffix}

  if name.len < fnam.len then begin    {FNAM ends with explicit image type suffix ?}
    ftype := '';                       {no specific type, will come from file name}
    return;
    end;

  ftype := 'GIF';                      {chose GIF file type by default}
  end;
{
******************************************************************
*
*   Start of main routine.
}
begin
  string_cmline_init;                  {init for reading the command line}

  util_mem_context_get (util_top_mem_context, mem_p); {create private mem context}
  mem_p^.pool_size := 32768;
  mem_p^.max_pool_chunk := 2048;
  get_suffixes ([file_rw_read_k], rsuff); {get fnam suffixes for readable images}
{
********************
*
*   Read the command line.
}
  oname_set := false;
  loop := true;
  waitf_set := false;
  waitl_set := false;
  alpha := false;
  infirst_p := nil;
  inlast_p := nil;
  nin := 0;
  rate := 10.0;
  waitf := 1.0 / rate;
  waitl := 1.0 / rate;
  zoom := 1;
{
*   Back here each new command line option.
}
next_opt:
  string_cmline_token (opt, stat);     {get next command line option name}
  if string_eos(stat) then goto done_opts; {exhausted command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  if (opt.len >= 1) and (opt.str[1] <> '-') then begin {implicit pathname token ?}
    if not oname_set then begin        {output file name not set yet ?}
      string_copy (opt, fnam_out);     {set output file name}
      oname_set := true;               {output file name is now set}
      goto next_opt;
      end;
    expand_patt (opt);                 {expand in images pattern and add to list}
    goto next_opt;
    end;
  string_upcase (opt);                 {make upper case for matching list}
  string_tkpick (opt, cmds, pick);     {pick option name keyword from list}
  case pick of                         {do routine for specific option}
{
*   -OUT filename
}
1: begin
  string_cmline_token (fnam_out, stat);
  oname_set := true;
  end;
{
*   -IN pattern
}
2: begin
  string_cmline_token (parm, stat);
  if sys_error(stat) then goto err_parm;
  expand_patt (parm);
  end;
{
*   -INLIST filename
}
3: begin
  string_cmline_token (parm, stat);    {get list file name in PARM}
  if sys_error(stat) then goto err_parm;
  file_open_read_text (parm, '', conn, stat); {open the list file}
  if sys_error(stat) then goto err_parm;
  expand_list (conn, stat);            {read and process the list file}
  file_close (conn);                   {close the list file}
  end;
{
*   -STDIN
}
4: begin
  file_open_stream_text (              {open connection to standard input stream}
    sys_sys_iounit_stdin_k,            {ID of stream to open connection to}
    [file_rw_read_k],                  {we only need read access to the stream}
    conn,                              {returned connection descriptor}
    stat);
  if sys_error(stat) then goto err_parm;
  expand_list (conn, stat);            {read and process the images list}
  file_close (conn);                   {close the stream connection}
  end;
{
*   -LOOP
}
5: begin
  loop := true;
  end;
{
*   -NLOOP
}
6: begin
  loop := false;
  end;
{
*   -RATE rate
}
7: begin
  string_cmline_token_fpm (rate, stat);
  end;
{
*   -WAITF sec
}
8: begin
  string_cmline_token_fpm (waitf, stat);
  waitf_set := true;
  end;
{
*   -WAITL sec
}
9: begin
  string_cmline_token_fpm (waitl, stat);
  waitl_set := true;
  end;
{
*   -FORM string
}
10: begin
  string_cmline_token (str, stat);     {read format string into STR}
  if sys_error(stat) then goto err_parm;
  p := 1;                              {init format string parse index}

  while true do begin                  {loop until exhausted STR tokens}
    string_token (str, p, parm, stat); {get next token from input string}
    if string_eos(stat) then exit;     {exhausted input string}
    if sys_error(stat) then goto err_parm;
    string_append_token (uform, parm); {add this token to end of user format string}
    end;
  end;
{
*   -ZOOM n
}
11: begin
  string_cmline_token_int (zoom, stat);
  if sys_error(stat) then goto err_parm;
  if zoom < 1 then goto parm_bad;
  end;
{
*   -ALPHA
}
12: begin
  alpha := true;
  end;
{
*   -NALPHA
}
13: begin
  alpha := false;
  end;
{
*   Unrecognized command line option.
}
otherwise
    string_cmline_opt_bad;             {unrecognized command line option}
    end;                               {end of command line option case statement}

err_parm:                              {jump here on parameter error, STAT set}
  string_cmline_parm_check (stat, opt); {check for bad command line option parameter}
  goto next_opt;                       {back for next command line option}

parm_bad:                              {jump here on bad parameter value}
  string_cmline_reuse;                 {re-read last command line token next time}
  string_cmline_token (parm, stat);    {re-read the token for the bad parameter}
  sys_msg_parm_vstr (msg_parm[1], parm);
  sys_msg_parm_vstr (msg_parm[2], opt);
  sys_message_bomb ('string', 'cmline_parm_bad', msg_parm, 2);

done_opts:                             {done with all the command line options}
{
*   All done reading the command line.
}
  if infirst_p = nil then begin        {no input frames ?}
    sys_message_bomb ('img', 'image_anim_no_in', nil, 0);
    end;

  if not oname_set then begin          {no output name specified ?}
    sys_message_bomb ('img', 'image_anim_no_out', nil, 0);
    end;

  if waitf_set then begin              {explicit duration for first frame ?}
    infirst_p^.sec := waitf;
    end;

  if waitl_set then begin              {explicit duration for last frame ?}
    inlast_p^.sec := waitl;
    end;
{
*   Open first image input file to gather some configuration information.
}
  img_open_read_img (infirst_p^.name_p^, img, stat);
  sys_error_abort (stat, '', '', nil, 0);

  x_size := img.x_size;                {save image sizes in pixels}
  y_size := img.y_size;
  aspect := img.aspect;                {save overall image aspect ratio}

  img_close (img, stat);               {close temp conn to first input image}
  sys_error_abort (stat, '', '', nil, 0);
{
********************
*
*   Open output image file.
}
  string_list_init (comm, mem_p^);     {init output image comment lines list}

  string_vstring (                     {init fixed part of format string}
    str,
    '-RED 8 -GREEN 8 -BLUE 8 -ANIM -RATE '(0),
    -1);
  string_f_fp_free (parm, 1.0 / infirst_p^.sec, 4);
  string_append (str, parm);
  if alpha then begin                  {enable output alpha component ?}
    string_appends (str, ' -ALPHA 8'(0));
    end;
  if loop then begin                   {animation is continuous loop ?}
    string_appends (str, ' -LOOP'(0));
    end;
  if uform.len > 0 then begin          {additional format commands from user ?}
    string_append1 (str, ' ');
    string_append (str, uform);
    end;

  make_ftype (fnam_out, ftype);        {set output image file type string in FTYPE}

  img_open_write_img (                 {open the output image file}
    fnam_out,                          {image file name}
    aspect,                            {width/height image aspect ratio}
    x_size * zoom, y_size * zoom,      {image size in pixels}
    ftype,                             {image file type string}
    str,                               {driver format string}
    comm,                              {list of comment lines}
    imgo,                              {returned connection to new image file}
    stat);
  sys_error_abort (stat, '', '', nil, 0);
{
*   The output image file is open on IMGO.
*
********************
}
  img_mem_alloc (                      {allocate mem for input scan line buffer}
    imgo,                              {connection to associate memory with}
    sizeof(scan_p^[0]) * x_size,       {amount of memory to allocate}
    scan_p);                           {returned pointer to the new memory}
  scano_p := scan_p;                   {init in and out scan buffers are the same}

  if zoom > 1 then begin               {need separate output scan line ?}
    img_mem_alloc (                    {allocate mem for input scan line buffer}
      imgo,                            {connection to associate memory with}
      sizeof(scano_p^[0]) * x_size * zoom, {amount of memory to allocate}
      scano_p);                        {returned pointer to the new memory}
    end;
{
*   Loop thru each input image and copy it to the output image file.
}
  in_p := infirst_p;                   {init to first image in input list}
  nframe := 0;                         {init current frame number}

  while in_p <> nil do begin           {once for each animation frame}
    nframe := nframe + 1;              {make 1-N number of this animation frame}
    img_open_read_img (in_p^.name_p^, img, stat); {open input image}
    sys_error_abort (stat, '', '', nil, 0);

    if (img.x_size <> x_size) or (img.y_size <> y_size) then begin {diff pix size ?}
      sys_msg_parm_vstr (msg_parm[1], in_p^.name_p^);
      sys_msg_parm_int (msg_parm[2], img.x_size);
      sys_msg_parm_int (msg_parm[3], img.y_size);
      sys_msg_parm_int (msg_parm[4], x_size);
      sys_msg_parm_int (msg_parm[5], y_size);
      sys_message_bomb ('img', 'image_anim_size_change', msg_parm, 5);
      end;

    sys_msg_parm_int (msg_parm[1], nframe); {announce this animation frame}
    sys_msg_parm_real (msg_parm[2], in_p^.sec);
    sys_msg_parm_vstr (msg_parm[3], img.tnam);
    sys_message_parms ('img', 'image_anim_frame', msg_parm, 3);

    if in_p <> infirst_p then begin    {this is not first image in output file ?}
      string_vstring (str, '-RATE '(0), -1);
      string_f_fp_free (parm, 1.0 / in_p^.sec, 4);
      string_append (str, parm);
      img_next_write_img (             {open new image in output image file}
        imgo,                          {output image file connection}
        x_size * zoom, y_size * zoom,  {image size in pixels}
        str,                           {driver format string}
        stat);
      sys_error_abort (stat, '', '', nil, 0);
      end;
{
*   The input and output image file connections have been all set up and
*   the parameters have been checked.  Now copy the pixels from the input
*   to the output image files.
}
    for y := 0 to y_size-1 do begin    {once for each scan line in the input image}
      img_read_scan1 (img, scan_p^, stat); {read input image scan line}
      sys_error_abort (stat, '', '', nil, 0);
      if zoom > 1 then begin           {need to replicate pixels horizontally ?}
        xo := 0;                       {init output scan line X coordinate}
        for x := 0 to x_size-1 do begin {once for each input scan line pixel}
          for i := 1 to zoom do begin  {loop for each replication of input pixel}
            scano_p^[xo] := scan_p^[x];
            xo := xo + 1;              {advance to next pixel in output scan buffer}
            end;
          end;                         {back to do next input scan line pixel}
        end;                           {done handling horizontal pixel replication}
      for i := 1 to zoom do begin      {once for each replication of this scan line}
        img_write_scan1 (imgo, scano_p^, stat); {write this output image scan line}
        sys_error_abort (stat, '', '', nil, 0);
        end;
      end;                             {back for next input image scan line}
{
*   Done writing this frame to the output file.
}
    img_close (img, stat);             {close this input image file}
    sys_error_abort (stat, '', '', nil, 0);
    in_p := in_p^.next_p;              {point to data for next input image}
    end;                               {back to handle this new input image}
{
*   Done writing all the frames to the output image file.
}
  img_close (imgo, stat);              {close the output image file}
  sys_error_abort (stat, '', '', nil, 0);
  end.

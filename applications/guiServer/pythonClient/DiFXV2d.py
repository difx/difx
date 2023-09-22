#\if DOXYGEN_IGNORE ############################################################
#                                                                              #
#   Copyright (C) 2016 by John Spitzak                                         #
#                                                                              #
#   This program is free software; you can redistribute it and/or modify       #
#   it under the terms of the GNU General Public License as published by       #
#   the Free Software Foundation; either version 3 of the License, or          #
#   (at your option) any later version.                                        #
#                                                                              #
#   This program is distributed in the hope that it will be useful,            #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#   GNU General Public License for more details.                               #
#                                                                              #
#   You should have received a copy of the GNU General Public License          #
#   along with this program; if not, write to the                              #
#   Free Software Foundation, Inc.,                                            #
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  #
#                                                                              #
#\endif ########################################################################
#<!---======================================================================--->
## \brief Class used to both read and write .v2d files.
#
#  This class provides .v2d file parser and writer, allowing an existing .v2d
#  file to be read, elements of it examined, changed, or deleted, and
#  new elements to be added.  It can also be used to create completely new .v2d data.
#  Valid .v2d file content can be generated in the form of a string.
#
#  \tableofcontents
#
#  In parsing .v2d data, the class makes no initial assumptions about the .v2d
#  content beyond adherence to a few basic formatting rules (described in the
#  <i>vex2difx</i> documentation <a href="http://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/vex2difx">here</a>).
#  These rules include:
#    <ul>
#    <li>Comments start with a "#" character and consume the rest of the line they are on
#    <li>There are global parameters that take the form "name = value"
#    <li>The values can be comma or colon-separated lists.
#    <li>There are "sections" that take the form "type name { [CONTENT] }"
#    <li>The sections can contain parameters that follow the global parameter rules
#    <li>Whitespace can appear anywhere, but it is only required in obvious locations,
#        such as between a section type and its name.
#    <li>Whitespace can also appear between items in a comma or colon-separated list.
#        This violates the specifications in the <i>vex2difx</i> documentation, at least
#        as I read them, but it does appear to be consistent with what <i>vex2difx</i>
#        will accept.
#    <li>There shouldn't be anything else in the file.
#    </ul>
#  This class does not care about the <i>content</i> of the .v2d data, only that it
#  follow these rules - it is quite comfortable with parameters or section types that
#  aren't actually part of the .v2d protocol.  This allows it to deal with changes to the
#  <i>vex2difx</i> software, and leaves it up to you to know what .v2d data should or can
#  contain.
#
#  The formatting of parsed .v2d data, including comments and whitespace, is maintained
#  such that the output .v2d data should pass a "diff" test with parsed input data -
#  only actual data changes will be flagged.  This is done by maintaining all .v2d
#  data as a collection of strings, which is a little strange and under some
#  circumstances might be considered inefficient.  Data extracted from or added to
#  the .v2d content is done using strings only.
#
#  \section usev2dclass Use of the Class
#
#  A parser instance is created with a string that contains .v2d data.  This can come
#  from a file, or whatever.  If the string is "None", completely empty .v2d data will
#  be initialized, waiting for you to add to it.
#
#  \code{.py}
#  import DiFXV2d
#
#  #  Create a class instance with a string (which in this example comes
#  #  from a file)
#  v2d = DiFXV2d.Parser( open( "r4744.v2d" ).read() )
#  
#  #  Create a class instance with "None".  This is empty .v2d data.
#  v2d = DiFXV2d.Parser( None )
#  \endcode
#
#  Functions can then be used to obtain/change data items in the .vex data - these
#  are detailed below.
#
#  You can produce a legal .v2d file from the parser content using the output()
#  function, which returns a string:
#
#  \code{.py}
#  out = open( "out.v2d", "w" )
#  out.write( v2d.output() )
#  out.close()
#  \endcode
#
#  \subsection internalDataOrg Internal Data Organization
#
#  Within the Parser class, all .v2d data, whether parsed from input data or
#  created and edited by the user, are maintained as a list of string "tokens" of different types.
#  There are only six of these types, representing the limited variety of
#  content in the .v2d data.  They include:
#  <ol>
#  <li>comments
#  <li>whitespace of all kinds
#  <li>open braces (used, with close braces, to enclose sections)
#  <li>close braces
#  <li>equal signs (used to set parameters)
#  <li>everything else
#  </ol>
#  The "everything else" type includes the names and types of sections and the
#  names and values of parameters - basically all of the meat of the .v2d data.
#  When the Parser locates any of these items to read or edit their values, it does
#  so using the index into the token list where they reside.  Any adding and
#  deleting of items is also done by index into the token list.
#
#  The Parser class provides functions that allow read, edit, delete, and add
#  access to items in the .v2d data using their names, such that the details of
#  the internal token list can be completely ignored.  However, the class itself
#  makes use of functions that return the index to items, and if you wish to
#  employ these they are available.  They just aren't very user-friendly.
#
#  \section extracting Extracting Information From the .v2d Data
#
#  The Parser provides a pretty complete list of functions that can be used to extract
#  items from .v2d data, all of which are documented below.  
#  There are a couple of rules to remember when using these functions.
#  <ol>
#  <li>All items are treated as strings, and maintained as strings.  This includes names
#      of parameters <i>as well as their values</i>.  A function that returns the value
#      of a parameter will return it as a string, and it is your responsibility to translate
#      that string into a double, list, integer, boolean, or whatever else.
#  <li>When no data exist that fit a request, None is returned.  A None return does not
#      mean anything more than what you asked for does not exist - no error checking for
#      "legal" (in the eyes of <i>vex2difx</i>) requests is done.  You can request the
#      value of a parameter named "NotThere" and the Parser will dutifully look for it (and
#      presumably not find it).  There is one exception to the None rule in the
#      getSectionParams() function - it will return None if a requested section does
#      not exist, but an empty list if the section is there but contains no parameters.
#  </ol>
#
#  Below is a somewhat simplistic representative .v2d file used in the documentation
#  that follows to illustrate functions that extract content from .v2d data.
#
#  \code{.py}
#  #   This is a sample .v2d file.
#  
#  vex = test.vex
#  
#  maxGap = 180000.0
#  maxLength = 360000.0
#  singleScan = true
#  startSeries = 1
#  
#  antennas = KK, MA, NY
#  
#  SETUP normalSetup {
#  }
#  
#  RULE scansubset {
#      setup = normalSetup
#  }
#  
#  ANTENNA KK {
#      phaseCalInt = 1
#      toneSelection = smart
#      file = /home/oper/r4758_01.data/kk/MED-0020_0002_r4758_kk_266-1937
#  }
#  
#  ANTENNA MA {
#      phaseCalInt = 1
#      toneSelection = smart
#      file = /home/oper/r4758_01.data/ma/USN-0141_0021_r4758_ma_266-1937
#  }
#  
#  ANTENNA NY {
#      phaseCalInt = 1
#      toneSelection = smart
#      file = /home/oper/r4758_01.data/ny/r4758_ny_266-1937.m5a
#  }
#  
#  \endcode
#
#  Beyond comments, the .v2d data can contain only three basic components - global parameter
#  settings (such as the "maxGap" and "singleScan" items above), section definitions ("RULE scansubset",
#  "ANTENNA MA", etc.), and parameter settings within sections ("setup" in "RULE scansubset", "phaseCalInt" 
#  in the "ANTENNA" sections, and others).  You can access
#  individual items by name, however in the event you don't know exactly what the data contain
#  a number of functions provide lists of components that are present.
#
#  The getGlobalNames() function provides a list of all global parameter names, in the
#  form of strings.
#
#  \code{.py}
#  >>> import DiFXV2d
#  >>> v2d = DiFXV2d.Parser( open( "test.v2d" ).read() )
#  >>> v2d.getGlobalNames()
#  ['vex', 'maxGap', 'maxLength', 'singleScan', 'startSeries', 'antennas']
#  >>> 
#  \endcode
#
#  The more generic getGlobalParams() function provides a list of tuples, each containing
#  a pair of strings representing the name and value of a global parameter.
#
#  \code{.py}
#  >>> v2d.getGlobalParams()
#  [('vex', 'test.vex'), ('maxGap', '180000.0'), ('maxLength', '360000.0'), ('singleScan', 'true'), ('startSeries', '1'), ('antennas', 'KK, MA, NY')]
#  >>> 
#  \endcode
#
#  The value of an individual global parameter can be found using the getGlobal()
#  function.  Remember, regardless of how <i>vex2difx</i> interprets this parameter,
#  the return from this function is a string (or None).
#
#  \code{.py}
#  >>> v2d.getGlobal( "maxLength" )
#  '360000.0'
#  >>> v2d.getGlobal( "NotThere" )
#  >>> 
#  \endcode
#
#  Sections in the .v2d data have both a name and a type.  Within a pair of braces,
#  a section can contain any number of parameters and their values.  The combination of
#  name and type are used to uniquely identify a section within the data, as there may be several
#  sections of the same type.  For instance, the above sample .v2d data
#  have several sections devoted to antennas, each having the "ANTENNA" type.
#  The getSectionsOfType() function can be used to obtain a list of sections that share
#  the same type.
#
#  \code{.py}
#  >>> v2d.getSectionsOfType( "ANTENNA" )
#  ['KK', 'MA', 'NY']
#  >>> 
#  \endcode
#
#  You can use the getSectionTypes() function to produce a list of all section types
#  that are present.
#
#  \code{.py}
#  >>> v2d.getSectionTypes()
#  ['SETUP', 'RULE', 'ANTENNA']
#  >>> 
#  \endcode
#
#  The getSections() function will generate a list of all section type/name pairs as
#  tuples.
#
#  \code{.py}
#  >>> v2d.getSections()
#  [('SETUP', 'normalSetup'), ('RULE', 'scansubset'), ('ANTENNA', 'KK'), ('ANTENNA', 'MA'), ('ANTENNA', 'NY')]
#  >>> 
#  \endcode
#
#  Within a specific section, there can be any number of parameter defintions (including
#  none of them).  The getSectionParams() function will provide a list of tuples, each
#  containing a name and value, for all of the parameters within a specified section.
#  Sections are specified by their name and value, which are given either as a tuple
#  or two string arguments.  The getSectionParams() function is slightly unusual in that
#  it will return None when a section doesn't exist, but an empty list when a section exists
#  but has no parameters (this is a departure from the practice of returning None when
#  requested items are absent that is employed by other functions).
#
#  \code{.py}
#  >>> #  You can provide a type and value as separate strings
#  ... v2d.getSectionParams( "ANTENNA", "NY" )
#  [('phaseCalInt', '1'), ('toneSelection', 'smart'), ('file', '/home/oper/r4758_01.data/ny/r4758_ny_266-1937.m5a')]
#  >>> #  Or you can use a tuple
#  ... v2d.getSectionParams( ( "ANTENNA", "NY" ) )
#  [('phaseCalInt', '1'), ('toneSelection', 'smart'), ('file', '/home/oper/r4758_01.data/ny/r4758_ny_266-1937.m5a')]
#  >>> #  Empty sections return empty lists
#  ... v2d.getSectionParams( "SETUP", "normalSetup" )
#  []
#  >>> #  But nonexistent sections return None
#  ... v2d.getSectionParams( "BOGUS", "NotThere" )
#  >>> 
#  \endcode
#
#  \section addingAndEditing Adding and Editing Data
#
#  There are really only three types of items that can be added or edited in .v2d data (excluding comments).
#  These are global parameters, sections, and parameters within sections.  Each of these items are identified
#  by a unique name in the case of parameters (both global and within sections) or a unique type/name pair
#  in the case of sections.  The Parser provides functions both to add and to edit items using their
#  unique names or type/name pairs.  The same functions perform both operations - if a requested item exists,
#  its value is changed, if it does not, it is created.  All functions can be used to add comments to the
#  .v2d data, as described \ref optionalComments "below".
#
#  Global variables are added or changed using the setGlobal() function.  It requires a name and value as
#  arguments.  If the named global is found in the data, it will be set to the new value.  If it is not found,
#  it will be added with the given value.
#
#  \code{.py}
#  >>> import DiFXV2d
#  >>> #  Create empty .v2d data
#  ... v2d = DiFXV2d.Parser()
#  >>> #  Add a global
#  ... v2d.setGlobal( "name", "value" )
#  >>> print v2d.output()
#  
#  name = value
#  >>> #  Change the value of existing global
#  ... v2d.setGlobal( "name", "different value" )
#  >>> print v2d.output()
#  
#  name = different value
#  >>> 
#  \endcode
#
#  As demonstrated by the silly name choices in the above example, the Parser only formats your request as a
#  global, it does not check to make sure that your parameter name is legal.  As always, the Parser assumes
#  you know what you are doing.
#
#  A section can be created using the createSection() function, which requires a section type and name as
#  arguments.  If the section already exists, this function will not create or alter it (however it can be
#  used to add a \ref optionalComments "comment").  The createSection() function, for internal reasons, returns
#  an integer (the index into the data token list, if you must know).  This can be ignored.
#
#  \code{.py}
#  >>> import DiFXV2d
#  >>> v2d = DiFXV2d.Parser()
#  >>> #  Create a new, empty section.
#  ... v2d.createSection( "NEWTYPE", "NEWNAME" )
#  1
#  >>> print v2d.output()
#  
#  
#  NEWTYPE NEWNAME {
#  }
#  >>>
#  \endcode
#
#  The setSectionParam() function can be used to set the value of a named parameter within a section secified
#  by type and name.  If the parameter exists, its value will be changed.  If it does not exist, it will be
#  created within the section.  If the section does not exist, the section will first be created.
#
#  \code{.py}
#  >>> import DiFXV2d
#  >>> v2d = DiFXV2d.Parser()
#  >>> #  Create a new parameter in a section that does not exist yet.
#  ... v2d.setSectionParam( "NEWTYPE", "NEWNAME", "varname", "varvalue" )
#  >>> print v2d.output()
#  
#  
#  NEWTYPE NEWNAME {
#      varname = varvalue
#  }
#  >>> #  Create a new parameter in an already existing section.
#  ... v2d.setSectionParam( "NEWTYPE", "NEWNAME", "anothervar", "varvalue" )
#  >>> print v2d.output()
#  
#  
#  NEWTYPE NEWNAME {
#      varname = varvalue
#      anothervar = varvalue
#  }
#  >>> #  Change an existing parameter in an existing section.
#  ... v2d.setSectionParam( "NEWTYPE", "NEWNAME", "anothervar", "newvalue" )
#  >>> print v2d.output()
#  
#  
#  NEWTYPE NEWNAME {
#      varname = varvalue
#      anothervar = newvalue
#  }
#  >>> 
#  \endcode
#
#  \subsection optionalComments Optional Comment Arguments
#
#  All of the "set/create" functions include an optional comment string that allows the
#  user to insert a comment prior to the parameter or section.  When comments
#  are inserted they will be indented to match the parameter (the user doesn't
#  have to do this).  If a parameter is being added, the comment will match
#  whatever indenting the formatting hints (see below) indicate.
#
#  The optional comment string does not need to contain the .v2d comment character.
#  When comments are added to edited or newly-created items, they are
#  prepended with a character sequence that you can set using setCommentSequence().
#  You can see what is being used with getCommentSequence().  By default the 
#  comment sequence is the comment character "#" followed by two spaces.
#  The spaces can be anything you like, but the comment character should not
#  be messed around with as it is in the .v2d specification and omitting it
#  will probably cause <i>vex2difx</i> to fail.
#
#  \code{.py}
#  >>> import DiFXV2d
#  >>> v2d = DiFXV2d.Parser()
#  >>> #  Trivial example using a "set" function with a comment.
#  ... v2d.setGlobal( "foo", "1", "this is a comment!" )
#  >>> print v2d.output()
#  
#  #  this is a comment!
#  foo = 1
#  >>> 
#  \endcode
#
#  When setting a parameter within a section where the section does not yet exist
#  and needs to be created, any included comment applies <i>only</i> to the
#  parameter:
#
#  \code{.py}
#  >>> import DiFXV2d
#  >>> v2d = DiFXV2d.Parser()
#  >>> #  Add a variable to a section that does not exist - comment only applies to the variable.
#  ... v2d.setSectionParam( "NEWSECTION", "NEWNAME", "foo", "1", "comment associated with variable" )
#  >>> print v2d.output()
#  
#  
#  NEWSECTION NEWNAME {
#      #  comment associated with variable
#      foo = 1
#  }
#  >>> #  Alternatively, if we create an empty section first, a comment can be added to that.
#  ... v2d.createSection( "NEWSECTION", "ANOTHERNAME", "this is a new section" )
#  17
#  >>> v2d.setSectionParam( "NEWSECTION", "ANOTHERNAME", "foo", "2", "comment with the variable" )
#  >>> print v2d.output()
#  
#  
#  NEWSECTION NEWNAME {
#      #  comment associated with variable
#      foo = 1
#  }
#  
#  #  this is a new section
#  NEWSECTION ANOTHERNAME {
#      #  comment with the variable
#      foo = 2
#  }
#  >>> 
#  \endcode
#
#  In the event that a parameter is changed, as opposed to added, a specified
#  comment will be added <i>only</i> if an exact match isn't already there.  This
#  should keep automated software from repeatedly inserting duplicate comments.
#
#  Here we set a parameter a couple of times, each time including a unique comment:
#
#  \code{.py}
#  import DiFXV2d
#  v2d = DiFXV2d.Parser()
#  #  Set a global value...
#  v2d.setGlobal( "foo", "1", "set the value to 1" )
#  #  Set it again with an appropriate comment...
#  v2d.setGlobal( "foo", "2", "actually should be 2" )
#  print v2d.output()
#  \endcode
#
#  Output from this activity will look like this.  Both comments are maintained:
#  
#  \code{.py}
#  #  set the value to 1
#  #  actually should be 2
#  foo = 2
#  \endcode
#
#  If we then change the value a few more times using the same comment over and over:
#
#  \code{.py}
#  #  Set it a few more times, but this time with identical comments
#  v2d.setGlobal( "foo", "bar", "this comment is identical and shouldn't be duplicated" )
#  v2d.setGlobal( "foo", "bar", "this comment is identical and shouldn't be duplicated" )
#  v2d.setGlobal( "foo", "bar", "this comment is identical and shouldn't be duplicated" )
#  print v2d.output()
#  \endcode
#
#  Our result will look like this.  The repeated comment is included only once:
#
#  \code{.py}
#  #  set the value to 1
#  #  actually should be 2
#  #  this comment is identical and shouldn't be duplicated
#  foo = bar
#  \endcode
#
#  \section deletingItems Deleting Data Items
#  
#  Items can be completely deleted from existing .v2d data.  To delete a global parameter,
#  the deleteGlobal() function is given a parameter name.  If the parameter exists in the
#  data (deleting a non-existent item is always harmless, although ineffective), the parameter
#  name, value, and the whitespace the precedes it will be deleted.  In addition, any
#  comments that are associated with the parameter are deleted.
#
#  How is it determined that a comment is associated with a parameter?  This is a bit
#  of a judgement call.  The Parser assumes that a comment is associated with a parameter
#  if it is on the immediately preceding line in the .v2d data AND it matches the indentation
#  of the parameter.  Any comments that match this description the precede the parameter
#  are considered associated unless they are separated by something else.  For instance,
#  the following comments are considered associated with a the parameter "foo", and will
#  all be deleted if "foo" is deleted:
#
#  \code{.py}
#      #  This is a comment about foo and will be deleted with it
#      #  And so is this
#      foo = someValue
#  \endcode
#
#  Some other comments are not considered associated.
#
#  \code{.py}
#      #  This comment is NOT associated, because there is a gap
#
#      #  This is a comment about foo and will be deleted with it
#      #  And so is this
#      foo = someValue
#  \endcode
#
#  \code{.py}
#      #  This comment is NOT associated because the comment below is not
#  #  This comment is NOT associated, because the indentation doesn't match
#      #  This is a comment about foo and will be deleted with it
#      #  And so is this
#      foo = someValue
#  \endcode
#
#  The same comment deletion paradigm applies to all item deletion functions in the Parser.
#  A comment is considered associated if it immmediately precedes the item you are deleting
#  and matches its indentation.
#
#  An entire section can be deleted using the deleteSection() function.  This function takes
#  either two string arguments representing the type and name of the offending section,
#  or a single tuple containing the same strings.  Any parameters contained in the section
#  are deleted as well.  If the section has associated comments, they will go too.
#
#  If you favor a more delicate touch, the deleteSectionParam() function can be used to
#  delete individual parameters within a section.  It takes either three string arguments: the
#  section type, section name, and parameter name, or a tuple containing the section type and
#  name along with a string for the parameter name.  As always, any associated comments are
#  deleted as well.
#
#  \section fieldComments Adding and Deleting "Field" Comments
#
#  In addition to comments that accompany sections or parameters, you may wish to
#  add or remove "field" comments in your .v2d data in locations not associated with a
#  specific component.  Field comments appear at the beginning or
#  end of the data - in "comment headers" or "footers".  The setComment() function can be used
#  to add a field comment.  In addition
#  to the comment itself, setComment() takes an optional argument specifying the
#  position where the comment should be added.  The position is a case-insensitive
#  string, and can have any of the following values:
#  <ul>
#  <li><b>TOP</b> will put the comment at the very top of the file - it will become
#      the first visible item in any output.
#  <li><b>HEAD</b> will add the comment at the "end" of any header comments that
#      already exist (this is identical to TOP if no header exists).  It determines
#      what comprises an existing header by looking at the beginning of the data for comments separated by whitespace
#      consisting of a single newline character.  As soon as it encounters anything
#      else, the new comment is inserted before it.
#  <li><b>FOOT</b> will add the comment at the end of the data.
#  </ul>
#
#  If you include no location argument, or a location argument that isn't in the
#  above list, the HEAD location will be used.
#  Comments are added with a simple newline as whitespace between themselves and
#  whatever follows, and will have no indentation.  
#
#  If a comment already exists that exactly matches what you are requesting, no
#  comment will be added.  This is to prevent automated software from adding
#  duplicate comments.
#
#  The comment text you specify need not include the .v2d comment character "#",
#  as any comment will be preceded by the "comment sequence" (which includes it).
#  You can see the comment sequence using the getCommentSequence() function, and
#  you can change it using the setCommentSequence() function.  When using the latter,
#  make sure the sequence includes the "#" character, otherwise you may corrupt your
#  .v2d data such that <i>vex2difx</i> will be unable to parse it correctly. 
#
#  You can examine comments that are already part of the data using the getComments()
#  function.  This function will find all comments in the data that contain a
#  given string sequence.  Like the setComment() function, it takes an optional
#  location argument, which may be either "HEAD" or "FOOT".  If present, only
#  comments in the given location will be returned.  If the argument is omitted,
#  all matching comments wherever they exist in the data will be returned.  The
#  return is a list of strings, or None if no matching items are found.
#
#  If you want to match all comments, an empty or None search string can be used.
#
#  To get rid of existing comments, the deleteComments() function can be used.
#  This function deletes all comments that contain a given string and match an
#  (optional) location.  If the location is not included, all matching comments
#  will be deleted.
#
#  \section formatting Formatting Considerations
#
#  The Parser goes through a certain amount of trouble to maintain as much of
#  the formatting of existing .v2d data as possible.  It does this by keeping
#  track of whitespace in the input data and duplicating it when tasked with
#  creating output.  This serves a dual purpose - it deliberately does not
#  annoy users with enforced formatting that they may not like, and it limits
#  changed parts of the data to the things that are actually changed - parameters
#  and sections edited, added, or deleted.
#
#  This works fine in the simple cases where existing parameters have their
#  values changed, however it gets trickier when items are deleted, and in
#  particular when new things are added.  How are new things to be formatted
#  such that, once again, the user isn't annoyed?
#
#  The parser tries to be cute about this by examining the existing data and
#  making a guess as to what already exists, then copying it.  It maintains
#  a number of formatting "hints" that are essentially a string of whitespace
#  that will be inserted prior to any newly-created item.  These hints 
#  (and what they are set to by default) are:
#  <ul>
#  <li>Whitespace before a global parameter (default is a single newline) 
#  <li>Whitespace before a section type (default is two newlines)
#  <li>Whitespace before a section name (default is one space)
#  <li>Whitespace before an open brace (default is one space)
#  <li>Whitespace before a close brace (default is a newline)
#  <li>Whitespace before a parameter in a section (default is a newline and four spaces)
#  <li>Whitespace before an equal sign (default is one space)
#  <li>Whitespace before a parameter value (default is one space)
#  </ul>
#  The hints are discovered by examining any input .v2d data and discovering
#  what, in each case, is done most often.  When no information that will
#  help in formulating a hint can be gleaned, my "sensible" defaults will
#  be used.  You
#  can at any time enforce your own hints using the following functions
#  (the names should make their actions obvious).  
#  Enforcing a hint will only apply to newly-inserted data.
#
#  <ul>
#  <li>setGlobalHint()
#  <li>setSectionTypeHint()
#  <li>setSectionNameHint()
#  <li>setOpenBraceHint()
#  <li>setCloseBraceHint()
#  <li>setSectionParameterHint()
#  <li>setEqualSignHint()
#  <li>setValueHint()
#  </ul>
#  
#  These functions don't actually check that everything you put in the
#  whitespace strings is actually whitespace, so you could insert comments
#  or whatever else in there if you desire (this isn't recommended as it
#  could kill your .v2d data in the eyes of <i>vex2difx</i>).  Note that the
#  functions for adding or editing parameters and sections allow you to insert comments
#  already, so there is really no reason to mess around in this way.
#
#  There are also corresponding "get" functions that return each of
#  these items if you want to look at them.
#
#  <ul>
#  <li>getGlobalHint()
#  <li>getSectionTypeHint()
#  <li>getSectionNameHint()
#  <li>getOpenBraceHint()
#  <li>getCloseBraceHint()
#  <li>getSectionParameterHint()
#  <li>getEqualSignHint()
#  <li>getValueHint()
#  </ul>
#
#  Obviously if you don't care about formatting (which is probable - the
#  whole purpose of this class is to keep you from having to look at
#  .v2d data) you can completely ignore it.  Your .v2d data will
#  always be parseable and legal to <i>vex2difx</i>.
#
#<!---======================================================================--->

class Parser:
    
    #<!------------------------------------------------------------------------>
    ##   The creation function can be given a string containing existing .v2d
    ##   data, but this is not critical.
    #<!------------------------------------------------------------------------>
    def __init__( self, v2dString = None ):
        self.tokenList = []
        if not v2dString == None:
            self.data( v2dString )
    
    COMMENT       = 0
    WHITESPACE    = 1
    STRING        = 2
    OPEN_BRACE    = 3
    CLOSE_BRACE   = 4
    EQUAL_SIGN    = 5
    
    globalHint = None
    sectionTypeHint = None
    sectionNameHint = None
    openBraceHint = None
    closeBraceHint = None
    sectionParameterHint = None
    equalSignHint = None
    valueHint = None
    commentSequence = None
    
    #<!------------------------------------------------------------------------>
    ##   Use the given string as the source of .v2d data.
    #<!------------------------------------------------------------------------>
    def data( self, v2dString ):
        #   Break the v2d data into a list of "tokens".  Each token is a tuple
        #   containing a token type and a string of data.  Token types include
        #   white space, strings (names, values, anything we can't identify),
        #   open and close braces, comments, and equal signs.  With the exception
        #   of comments (which last until a newline), the beginning of any token
        #   type signifies the end of the previous token.
        self.tokenList = []
        #   The "type" to begin with is whitespace.
        tokenType = self.WHITESPACE
        tokenData = ""
        usingConnector = False
        i = 0
        while i < len( v2dString ):
            #  Test each character and decide what to do based on what sort of
            #  token type we are already within.  First there is whitespace...
            if not usingConnector and ( v2dString[i] == ' ' or v2dString[i] == '\t' or v2dString[i] == '\n' ):
                #  Add to whitespace if that's what exists...
                if tokenType == self.WHITESPACE:
                    tokenData = tokenData + v2dString[i]
                #  Otherwise, trigger the start of a new whitespace token
                else:
                    if len( tokenData ) > 0:
                        self.tokenList.append( ( tokenType, tokenData ) )
                    tokenType = self.WHITESPACE
                    tokenData = v2dString[i]
            #   Comment characters indicate a comment to the end of the current
            #   line, so we just consume the line to the end.
            elif v2dString[i] == "#":
                #  Save the current token
                if len( tokenData ) > 0:
                    self.tokenList.append( ( tokenType, tokenData ) )
                #  Save the rest of the line as a comment.  We have to watch for
                #  the end of file here, in case that happens.
                tokenData = v2dString[i]
                i = i + 1
                while not v2dString[i] == '\n' and i < len( v2dString ):
                    tokenData = tokenData + v2dString[i]
                    i = i + 1
                #  Add a newline to the token data unless we've reached the file end,
                #  then save the token.
                #if not i == len( v2dString ):
                #    tokenData = tokenData + "\n"
                #  If we hit a newline character, back up one index so it will become
                #  part of the subsequent whitespace token.
                if not i == len( v2dString ):
                    i = i - 1
                self.tokenList.append( ( self.COMMENT, tokenData ) )
                tokenData = ""
            #   There are three single-character tokens...
            elif v2dString[i] == "{" or v2dString[i] == "}" or v2dString[i] == "=":
                if len( tokenData ) > 0:
                    self.tokenList.append( ( tokenType, tokenData ) )
                if v2dString[i] == "{":
                    self.tokenList.append( ( self.OPEN_BRACE, "{" ) )
                elif v2dString[i] == "}":
                    self.tokenList.append( ( self.CLOSE_BRACE, "}" ) )
                else:
                    self.tokenList.append( ( self.EQUAL_SIGN, "=" ) )
                tokenData = ""
            #  Anything else is "string" data, which is all the meat of the .v2d
            #  file - section names, values, parameter names, etc.
            else:
                #  Are we already working on a string?
                if tokenType == self.STRING:
                    tokenData = tokenData + v2dString[i]
                #  Otherwise, start a new string
                else:
                    if len( tokenData ) > 0:
                        self.tokenList.append( ( tokenType, tokenData ) )
                    tokenType = self.STRING
                    tokenData = v2dString[i]
                #  Connectors include commas and colons.  These allow whitespace to be
                #  included in lists of items that should be strings.
                if v2dString[i] == "," or v2dString[i] == ":":
                    usingConnector = True
                else:
                    usingConnector = False
            i = i + 1
        if len( tokenData ) > 0 and not tokenType == None:
            self.tokenList.append( ( tokenType, tokenData ) )
        
    #<!------------------------------------------------------------------------>
    ## Create an output string of the .v2d data.
    #
    #  @return String of the complete .v2d data.
    #
    #  The current .v2d data is assembled into a single string that is returned
    #  as output.  If there is no data, None is returned as opposed to an empty
    #  string.
    #<!------------------------------------------------------------------------>
    def output( self ):
        if len( self.tokenList ) < 1:
            return None
        outputString = ""
        for item in self.tokenList:
            outputString = outputString + item[1]
        return outputString
        
    #<!------------------------------------------------------------------------>
    ## Obtain the location of a specified global parameter, if it exists.
    #
    #  @param name Name of the global parameter
    #  @return     Tuple of the index of the global parameter name and the
    #              index of the value.  None if the global parameter doesn't exist.
    #
    #  This function locates a "global" parameter within the existing .v2d
    #  data and returns a tuple containing the index of its name and index of 
    #  its value in the token list.  If the named global parameter does not exist
    #  in the current data, None is returned.
    #<!------------------------------------------------------------------------>
    def locateGlobal( self, name ):
        #  Why this would happen I have no idea, but might as well accommodate...
        if name == None:
            return None
        #  Try to locate the name in the list of token strings.  It's a global
        #  parameter so it must not be within braces.
        bracesLevel = 0
        afterEqual = False
        for i in range( len( self.tokenList ) ):
            item = self.tokenList[i]
            if item[0] == self.OPEN_BRACE:
                bracesLevel = bracesLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif item[0] == self.EQUAL_SIGN:
                afterEqual = True
            elif bracesLevel == 0 and item[0] == self.STRING and not afterEqual:
                #  Is this the right name?
                if item[1] == name:
                    #  Locate the next string (accommodating possible whitespace
                    #  and comments).  This should be the value, assuming the .v2d
                    #  source is formatted correctly.
                    look = i + 1
                    while look < len( self.tokenList ) and not self.tokenList[look][0] == self.STRING:
                        look = look + 1
                    if self.tokenList[look][0] == self.STRING:
                        return ( i, look )
            if afterEqual and item[0] == self.STRING:
                afterEqual = False
        #  If we reached here, nothing was found.
        return None
        
    #<!------------------------------------------------------------------------>
    ## Get the index within the token list of a specified section.
    #
    #  @param sType Type of the section
    #  @param name Name of the section
    #  @return     Integer index of the section within the token list.  None
    #              is returned if the parameter is not found.
    #
    #  This function locates a section of given type and name within the
    #  internal token list and returns the index of it within that list.  None
    #  will be returned of the name/type section cannot be found in the data.
    #  This function was developed to be used internally, but might be useful
    #  for determining if a section exists.
    #<!------------------------------------------------------------------------>
    def getSectionIndex( self, sType, name = None ):
        #  Split up a tuple of the first argument is one.
        if name == None:
            name = sType[1]
            sType = sType[0]
        #  Input sanity check
        if name == None or sType == None:
            return None
        #  Locate the section type in the token strings.  It appears first.  We
        #  count braces to make sure we are not within braces when the type and
        #  name are located (just in case the .v2d data contain something insidious).
        bracesLevel = 0
        for i in range( len( self.tokenList ) ):
            item = self.tokenList[i]
            if item[0] == self.OPEN_BRACE:
                bracesLevel = bracesLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif bracesLevel == 0 and item[0] == self.STRING and item[1] == sType:
                #  Locate the next string (accommodating possible whitespace
                #  and comments).  This can be compared to the name.
                look = i + 1
                while look < len( self.tokenList ) and not self.tokenList[look][0] == self.STRING:
                    look = look + 1
                if self.tokenList[look][0] == self.STRING and self.tokenList[look][1] == name:
                    return i
        #  If we reached here, nothing was found.
        return None
        
    #<!------------------------------------------------------------------------>
    ## Obtain the value of a specified global parameter, if it exists.
    #
    #  @param name Name of the global parameter
    #  @return     Value associated with the global parameter, if it exists.
    #              All values are returned as strings.  None is returned if
    #              the parameter is not found.
    #
    #  This function locates a "global" parameter within the existing .v2d
    #  file data.  Global parameters lie outside of sections.  If the named
    #  global parameter does not exist in the current data, None is returned.
    #  All existing values are returned as strings, exactly as they appeared
    #  in any input .v2d file or will appear in .v2d output.
    #
    #  No effort is made to assure that a name is "legal" within the .v2d
    #  framework (i.e. something <i>vex2difx</i> would understand).
    #<!------------------------------------------------------------------------>
    def getGlobal( self, name ):
        #  This is fairly simple - the bulk of the work is done in the
        #  getGlobalIndex() function.
        index = self.locateGlobal( name )
        if index == None:
            return None
        return self.tokenList[index[1]][1]
        
    #<!------------------------------------------------------------------------>
    ## Get a list of all global parameter names and values in the current data.
    #
    #  @return     A list tuples containing name and value pairs (as strings).
    #
    #  This function locates all global parameters within the existing .v2d
    #  data and returns a list of tuples, each containing a name and value
    #  (in string form) for each global.  If there are no global values None
    #  is returned.
    #<!------------------------------------------------------------------------>
    def getGlobalParams( self ):
        ret = []
        #  Locate all globals in the data by looking for pairs of strings outside
        #  braces that have an equal sign between them.
        bracesLevel = 0
        for i in range( len( self.tokenList ) ):
            item = self.tokenList[i]
            if item[0] == self.OPEN_BRACE:
                bracesLevel = bracesLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif bracesLevel == 0 and item[0] == self.STRING:
                #  Got a string, locate the next non-comment, non-whitespace thing.
                look = i + 1
                while look < len( self.tokenList ) and ( self.tokenList[look][0] == self.COMMENT or self.tokenList[look][0] == self.WHITESPACE ):
                    look = look + 1
                #  If it was an equal sign, advance to the next thing.
                if look < len( self.tokenList ) and self.tokenList[look][0] == self.EQUAL_SIGN:
                    look = look + 1
                    while look < len( self.tokenList ) and ( self.tokenList[look][0] == self.COMMENT or self.tokenList[look][0] == self.WHITESPACE ):
                        look = look + 1
                    #  If that's a string, its a value.  Save the name and value to the list.
                    if self.tokenList[look][0] == self.STRING:
                        ret.append( ( self.tokenList[i][1], self.tokenList[look][1] ) )
        #  Make sure we return None if nothing was found.  Otherwise return the
        #  list we made.
        if len( ret ) == 0:
            return None
        else:
            return ret
        
    #<!------------------------------------------------------------------------>
    ## Get a list of all global parameter names in the current data.
    #
    #  @return     A list strings containing global parameter names.
    #
    #  This function locates all global parameters within the existing .v2d
    #  data and returns a list of their names.  If there are no global values None
    #  is returned.
    #<!------------------------------------------------------------------------>
    def getGlobalNames( self ):
        ret = []
        namevals = self.getGlobalParams()
        if namevals == None:
            return None
        for item in namevals:
            ret.append( item[0] )
        #  Make sure we return None if nothing was found.  Otherwise return the
        #  list we made.
        if len( ret ) == 0:
            return None
        else:
            return ret
        
    #<!------------------------------------------------------------------------>
    ## Get a list of all section types.
    #
    #  @return     List of strings containing the unique types of sections within the
    #              .v2d data.  This will be None if there are none in the
    #              existing .v2d data.
    #
    #  This function locates all of the sections in the existing .v2d data and
    #  makes a list of the different types.  Types are not duplicated.  None will be returned
    #  if there are none.
    #<!------------------------------------------------------------------------>
    def getSectionTypes( self ):
        ret = []
        #  Locate section types in the token strings.
        bracesLevel = 0
        valueExpected = False
        for i in range( len( self.tokenList ) ):
            item = self.tokenList[i]
            if item[0] == self.OPEN_BRACE:
                bracesLevel = bracesLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif item[0] == self.EQUAL_SIGN:
                valueExpected = True
            elif item[0] == self.STRING:
                #  This might be a value...
                if valueExpected:
                    valueExpected = False
                elif bracesLevel == 0:
                    #  If the next non-whitespace, non-comment item is a string, this is a type.
                    look = i + 1
                    while look < len( self.tokenList ) and ( self.tokenList[look][0] == self.WHITESPACE or self.tokenList[look][0] == self.COMMENT ):
                        look = look + 1
                    if look < len( self.tokenList ) and self.tokenList[look][0] == self.STRING:
                        #  Only add this type if it is new
                        try:
                            foo = ret.index( item[1] )
                        except ValueError:
                            ret.append( item[1] )
        if len( ret ) == 0:
            ret = None
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Get a list of tuples containing the types and names of all sections.
    #
    #  @return     List of tuples containing string representations of all
    #              section types and names.  This will be None if there are none in the
    #              existing .v2d data.
    #
    #  This function locates all of the sections in the existing .v2d data and
    #  makes a list of tuples containing their types and names.  None will be returned
    #  if there are none.
    #<!------------------------------------------------------------------------>
    def getSections( self ):
        ret = []
        #  Locate section types in the token strings.
        bracesLevel = 0
        valueExpected = False
        for i in range( len( self.tokenList ) ):
            item = self.tokenList[i]
            if item[0] == self.OPEN_BRACE:
                bracesLevel = bracesLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif item[0] == self.EQUAL_SIGN:
                valueExpected = True
            elif item[0] == self.STRING:
                #  This might be a value...
                if valueExpected:
                    valueExpected = False
                elif bracesLevel == 0:
                    #  If the next non-whitespace, non-comment item is a string, this is a type.
                    look = i + 1
                    while look < len( self.tokenList ) and ( self.tokenList[look][0] == self.WHITESPACE or self.tokenList[look][0] == self.COMMENT ):
                        look = look + 1
                    if look < len( self.tokenList ) and self.tokenList[look][0] == self.STRING:
                        #  Only add this type if it is new
                        ret.append( ( item[1], self.tokenList[look][1] ) )
        if len( ret ) == 0:
            ret = None
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Get a list of section names that have the given type.
    #
    #  @param name Type specification
    #  @return     List of strings containing the names of sections with the
    #              given type.  This will be None if there are none in the
    #              existing .v2d data.
    #
    #  This function locates all of the sections that have the given type and
    #  returns a list of their names, in string form.  None will be returned
    #  if there are none.
    #<!------------------------------------------------------------------------>
    def getSectionsOfType( self, typeSpec ):
        ret = []
        #  Locate section types in the token strings.
        bracesLevel = 0
        valueExpected = False
        for i in range( len( self.tokenList ) ):
            item = self.tokenList[i]
            if item[0] == self.OPEN_BRACE:
                bracesLevel = bracesLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif item[0] == self.EQUAL_SIGN:
                valueExpected = True
            elif item[0] == self.STRING:
                #  This might be a value...
                if valueExpected:
                    valueExpected = False
                elif bracesLevel == 0 and item[1] == typeSpec:
                    #  If the next non-whitespace, non-comment item is a string, this is a type.
                    look = i + 1
                    while look < len( self.tokenList ) and ( self.tokenList[look][0] == self.WHITESPACE or self.tokenList[look][0] == self.COMMENT ):
                        look = look + 1
                    if look < len( self.tokenList ) and self.tokenList[look][0] == self.STRING:
                        ret.append( self.tokenList[look][1] )
        if len( ret ) == 0:
            ret = None
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Get a list of the parameters within a specified section.
    #
    #  @param sType Type of the section (string) or tuple containing type and name
    #  @param sName Name of the section (unless previous argument is a tuple)
    #  @return     A list of tuples containing the names and values of all
    #              parameters in the section.  None is returned if
    #              the section is not found, however an empty list is returned
    #              if the section exists but is empty.
    #
    #  This function locates all parameters within a specified section and
    #  returns their names and values as a list of tuples.  Sections are specifed by type and name,
    #  either as a tuple or by two separate strings.  If the section doesn't exist, None will be
    #  returned.  If the section exists but does not contain any parameters,
    #  an empty list will be returned.
    #<!------------------------------------------------------------------------>
    def getSectionParams( self, sType, sName = None ):
        #  If the second argument is None, treat the first as a tuple containing the
        #  type and name.
        if sName == None:
            sName = sType[1]
            sType = sType[0]
        #  Input sanity check
        if sType == None or sName == None:
            return None
        #  Get the index of the specified section, if it exists
        i = self.getSectionIndex( sType, sName )
        if i == None:
            return None
        ret = []
        #  The index should be pointing to the type.  Skip the name and the
        #  following opening brace.
        while i < len( self.tokenList ) and not self.tokenList[i][0] == self.STRING:
            i = i + 1
        if not i < len( self.tokenList ):
            return None
        while i < len( self.tokenList ) and not self.tokenList[i][0] == self.OPEN_BRACE:
            i = i + 1
        if not i < len( self.tokenList ):
            return None
        #  As long as we are within the braces, locate parameter names
        bracesLevel = 1
        while i < len( self.tokenList ) and bracesLevel > 0:
            item = self.tokenList[i]
            if item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif item[0] == self.STRING:
                #  Find the value, which should be the first string following the name
                i = i + 1
                while i < len( self.tokenList ) and not self.tokenList[i][0] == self.STRING:
                    i = i + 1
                if self.tokenList[i][0] == self.STRING:
                    ret.append( ( item[1], self.tokenList[i][1] ) )
            i = i + 1
        return ret
            
    #<!------------------------------------------------------------------------>
    ## Get the value of a parameter within a specified section.
    #
    #  @param sType Type of the section (string) or tuple containing type and name
    #  @param sName Name of the section (unless previous argument is a tuple)
    #  @param name Name of the parameter
    #  @return     Value associated with the parameter, if it exists.
    #              All values are returned as strings.  None is returned if
    #              the section or parameter within it is not found.
    #
    #  This function locates a parameter within a specified section and
    #  returns its value as a string.  Sections are specifed by type and name,
    #  parameters by their name.  If the section doesn't exist, None will be
    #  returned.  If the section exists but does not contain the parameter,
    #  None will be returned.
    #<!------------------------------------------------------------------------>
    def getSectionParam( self, sType, sName, name = None ):
        #  If the third argument is None, treat the first as a tuple containing the
        #  type and name of the section and the second as the name of the parameter.
        if name == None:
            name = sName
            sName = sType[1]
            sType = sType[0]
        #  Input sanity check
        if sType == None or sName == None or name == None:
            return None
        #  Get the index of the specified section, if it exists
        i = self.getSectionIndex( sType, sName )
        if i == None:
            return None
        #  The index should be pointing to the type.  Skip the name and the
        #  following opening brace.
        while i < len( self.tokenList ) and not self.tokenList[i][0] == self.STRING:
            i = i + 1
        if not i < len( self.tokenList ):
            return None
        while i < len( self.tokenList ) and not self.tokenList[i][0] == self.OPEN_BRACE:
            i = i + 1
        if not i < len( self.tokenList ):
            return None
        #  As long as we are within the braces, locate the name of the parameter
        bracesLevel = 1
        while i < len( self.tokenList ) and bracesLevel > 0:
            item = self.tokenList[i]
            if item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif item[0] == self.STRING and item[1] == name:
                #  Find the value, which should be the first string following the name
                i = i + 1
                while i < len( self.tokenList ) and not self.tokenList[i][0] == self.STRING:
                    i = i + 1
                if self.tokenList[i][0] == self.STRING:
                    return self.tokenList[i][1]
            i = i + 1
        return None
            
    #<!------------------------------------------------------------------------>
    ## Change or add a specified global parameter.
    #
    #  @param name  Name of the global parameter 
    #  @param value Value that will be associated with the parameter
    #  @param comment Optional comment that will be inserted before the parameter.
    #
    #  This function sets a global parameter in the .v2d data to a specified
    #  value.  If the parameter already exists it is changed, if not it will
    #  be added.  Values are converted to strings before being set.  An optional
    #  comment field will be inserted before the parameter (the comment
    #  character will be added by this function).
    #
    #  If the parameter is new, the question arises - where to put it?  At the
    #  moment the enforced behavior, which hopefully won't irritate anyone, is
    #  that the new global parameter declaration will be added at the end of any
    #  other global declarations that precede any section declarations.
    #
    #  No effort is made to assure that a name and/or value are "legal" within the .v2d
    #  framework (i.e. something <i>vex2difx</i> would understand).  That's up
    #  to you.
    #<!------------------------------------------------------------------------>
    def setGlobal( self, name, value, comment = None ):
        #  Get some strings we'll need - operations below can change them on the fly
        #  so we grab them first for stability.
        gotGlobalHint = self.getGlobalHint()
        gotEqualSignHint = self.getEqualSignHint()
        gotValueHint = self.getValueHint()
        gotCommentSequence = self.getCommentSequence()
        #  Locate this global declaration if it exists
        index = self.locateGlobal( name )
        #  If it does exist, the change is easy.
        if not index == None:
            self.tokenList[index[1]] = ( self.STRING, str( value ) )
            commentLoc = index[0]
        else:
            #  If not, add a new global variable.  The global is added just before
            #  The first section definition, but after the last global parameter.
            #  Find the first section by locating the first open brace.
            i = 0;
            while i < len( self.tokenList ) and not self.tokenList[i][0] == self.OPEN_BRACE:
                i = i + 1
            #  If we found an open brace, back up to the white space before the section
            #  definition.  This involves skipping two strings - the section type and
            #  section name.
            if len( self.tokenList ) > 0 and self.tokenList[i][0] == self.OPEN_BRACE:
                while not i < 0 and not self.tokenList[i][0] == self.STRING:
                    i = i - 1
                if i > 0:
                    i = i - 1
                while not i < 0 and not self.tokenList[i][0] == self.STRING:
                    i = i - 1
                if i > 0:
                    i = i - 1
            #  Now, locate the previous string.  This should be the last global value.
            while not i == 0 and not self.tokenList[i][0] == self.STRING:
                i = i - 1
            #  Find the "insert location".  Stuff is inserted before the insert location.
            #  If the we aren't at the start of the data (which would have happened if
            #  there were no data at all), move to the location just after the last
            #  string (assuming there is somewhere to move).
            if len( self.tokenList ) > 0 and self.tokenList[i][0] == self.STRING and i + 1 < len( self.tokenList ):
                i = i + 1
            #  Insert the global parameter whitespace "hint".
            self.tokenList.insert( i, ( self.WHITESPACE, gotGlobalHint ) )
            i = i + 1
            #  Save this location for the comment insertion
            commentLoc = i
            #  Insert the global parameter name, and equal sign, and a value, along with
            #  appropriate whitespace.
            self.tokenList.insert( i, ( self.STRING, name ) )
            i = i + 1
            self.tokenList.insert( i, ( self.WHITESPACE, gotEqualSignHint ) )
            i = i + 1
            self.tokenList.insert( i, ( self.EQUAL_SIGN, "=" ) )
            i = i + 1
            self.tokenList.insert( i, ( self.WHITESPACE, gotValueHint ) )
            i = i + 1
            self.tokenList.insert( i, ( self.STRING, value ) )
        #  If a comment is specified, add it followed by whitespace that is a single newline
        #  and whatever indentation follows the last newline of the pre-global whitespace.
        if not comment == None:
            #  First makes sure that an exact duplicate of this comment does not already
            #  exist.  This might happen if whatever software is doing this has already
            #  edited the same parameter.
            i = commentLoc - 1
            if i < 1 or not self.tokenList[i-1][0] == self.COMMENT or not self.tokenList[i-1][1] == gotCommentSequence + comment:
                i = commentLoc
                self.tokenList.insert( i, ( self.COMMENT, gotCommentSequence + comment ) )
                i = i + 1
                sub = gotGlobalHint.rfind( "\n" )
                if sub == -1:
                    self.tokenList.insert( i, ( self.WHITESPACE, "\n" + gotGlobalHint ) )
                else:
                    self.tokenList.insert( i, ( self.WHITESPACE, "\n" + gotGlobalHint[sub+1:] ) )

    #<!------------------------------------------------------------------------>
    ## Create a new section with specified type and name.
    #
    #  @param sType Type of the section
    #  @param sName Name of the section
    #  @param comment Optional comment that will inserted before the section.
    #  @return The index of the new (or pre-existing) section in the token list.  For internal use.
    #
    #  This function adds a new section of given type and name to the .v2d data.
    #  An optional comment is added prior to the section.  If the section already
    #  exists the function will not create it, however it will add the comment
    #  prior to it (unless an exact duplicate of the comment already exists).
    #
    #  New sections are put at the end of the .v2d data.
    #<!------------------------------------------------------------------------>
    def createSection( self, sType, sName, comment = None ):
        gotSectionTypeHint = self.getSectionTypeHint()
        gotSectionNameHint = self.getSectionNameHint()
        gotOpenBraceHint = self.getOpenBraceHint()
        gotCloseBraceHint = self.getCloseBraceHint()
        gotCommentSequence = self.getCommentSequence()
        #  See if the section already exists.
        index = self.getSectionIndex( sType, sName )
        if index == None:
            #  We have to create it.  Find the location of the last section, so we
            #  can put this one after it.
            sections = self.getSections()
            #  If there are no sections, put this after the last global.
            if sections == None:
                #  Find the last global.
                globals = self.getGlobalNames()
                #  If there are none, we have nothing to work with, so just put the
                #  new section at the end of the current data.
                if globals == None:
                    index = len( self.tokenList )
                else:
                    #  Find the last global and add just beyond it.
                    index = self.getGlobalIndex( globals[len(globals) - 1] )
                    #  Advance beyond the value
                    while index < len( self.tokenList ) and not self.tokenList[index][0] == self.EQUAL_SIGN:
                        index = index + 1
                    while index < len( self.tokenList ) and not self.tokenList[index][0] == self.STRING:
                        index = index + 1
                    if index < len( self.tokenList ):
                        index = index + 1
            else:
                #  Guessing where the user wants this new section...find the last section in the list
                #  that has a matching type.
                testSection = len( sections ) - 1
                while testSection > 0 and not sections[testSection][0] == sType:
                    testSection = testSection - 1
                if testSection > 0:
                    index = self.getSectionIndex( sections[testSection] )
                else:
                    #  Didn't find any of the same type - stick it after the last section
                    index = self.getSectionIndex( sections[len(sections) - 1] )
                #  Locate the closing brace at the end of this section.
                while index < len( self.tokenList ) and not self.tokenList[index][0] == self.CLOSE_BRACE:
                    index = index + 1
                if index < len( self.tokenList ):
                    index = index + 1
            #  Insert or append the new section depending on whether we have reached the
            #  end of the data.
            if index == len( self.tokenList ):
                #  Append the section to the end of the data.
                self.tokenList.append( ( self.WHITESPACE, gotSectionTypeHint ) )
                self.tokenList.append( ( self.STRING, sType ) )
                self.tokenList.append( ( self.WHITESPACE, gotSectionNameHint ) )
                self.tokenList.append( ( self.STRING, sName ) )
                self.tokenList.append( ( self.WHITESPACE, gotOpenBraceHint ) )
                self.tokenList.append( ( self.OPEN_BRACE, "{" ) )
                self.tokenList.append( ( self.WHITESPACE, gotCloseBraceHint ) )
                self.tokenList.append( ( self.CLOSE_BRACE, "}" ) )
            else:
                #  Insert the section if we are not at the end.
                i = index
                self.tokenList.insert( i, ( self.WHITESPACE, gotSectionTypeHint ) )
                i = i + 1
                self.tokenList.insert( i, ( self.STRING, sType ) )
                i = i + 1
                self.tokenList.insert( i, ( self.WHITESPACE, gotSectionNameHint ) )
                i = i + 1
                self.tokenList.insert( i, ( self.STRING, sName ) )
                i = i + 1
                self.tokenList.insert( i, ( self.WHITESPACE, gotOpenBraceHint ) )
                i = i + 1
                self.tokenList.insert( i, ( self.OPEN_BRACE, "{" ) )
                i = i + 1
                self.tokenList.insert( i, ( self.WHITESPACE, gotCloseBraceHint ) )
                i = i + 1
                self.tokenList.insert( i, ( self.CLOSE_BRACE, "}" ) )
            index = index + 1
        #  The section is now created.  Add a comment if that is requested.
        if not comment == None:
            #  The location of the comment is just before the section location with a
            #  whitespace separator.
            commentLoc = index
            i = commentLoc - 1
            if i < 1 or not self.tokenList[i-1][0] == self.COMMENT or not self.tokenList[i-1][1] == gotCommentSequence + comment:
                i = commentLoc
                self.tokenList.insert( i, ( self.COMMENT, gotCommentSequence + comment ) )
                i = i + 1
                sub = gotSectionTypeHint.rfind( "\n" )
                if sub == -1:
                    self.tokenList.insert( i, ( self.WHITESPACE, "\n" + gotSectionTypeHint ) )
                else:
                    self.tokenList.insert( i, ( self.WHITESPACE, "\n" + gotSectionTypeHint[sub+1:] ) )            
        return index
        
    #<!------------------------------------------------------------------------>
    ## Change or add a parameter within a section.
    #
    #  @param sType Type of the section
    #  @param sName Name of the section
    #  @param name  Name of the global parameter 
    #  @param value Value that will be associated with the parameter
    #  @param comment Optional comment that will inserted before the parameter.
    #
    #  This function sets a parameter within a specified section in the .v2d data.  
    #  If the parameter already exists it is changed, if not it will
    #  be added.  If the section does not exist, it will be created.  An optional
    #  comment field will be inserted before the parameter (the comment
    #  character will be added by this function).  If the section has to be
    #  created it is not possible to add a comment to it - that should be done
    #  with the createSection() function.
    #
    #  New parameters are placed at the end of any existing parameters within
    #  a section.
    #<!------------------------------------------------------------------------>
    def setSectionParam( self, sType, sName, name, value, comment = None ):
        gotSectionParamHint = self.getSectionParameterHint()
        gotEqualSignHint = self.getEqualSignHint()
        gotValueHint = self.getValueHint()
        gotCommentSequence = self.getCommentSequence()
        #  Get the index to the section, creating it if necessary.
        index = self.createSection( sType, sName )
        #  The index should be pointing to the type.  Skip the name and the
        #  following opening brace.
        i = index
        while i < len( self.tokenList ) and not self.tokenList[i][0] == self.STRING:
            i = i + 1
        if not i < len( self.tokenList ):
            return
        while i < len( self.tokenList ) and not self.tokenList[i][0] == self.OPEN_BRACE:
            i = i + 1
        if not i < len( self.tokenList ):
            return
        index = i
        #  Locate the closing brace, or the parameter name, if it is there.
        breakOut = False
        afterEqual = False
        varFound = False
        while i < len( self.tokenList ) and not breakOut:
            item = self.tokenList[i]
            if item[0] == self.CLOSE_BRACE:
                breakOut = True
            elif item[0] == self.STRING:
                #  See if this is before an equal sign (and thus a name), then see if it
                #  matches.
                if not afterEqual:
                    if item[1] == name:
                        commentLoc = i
                        #  Find and change the value, which should be the first string following the name
                        i = i + 1
                        while i < len( self.tokenList ) and not self.tokenList[i][0] == self.STRING:
                            i = i + 1
                        if self.tokenList[i][0] == self.STRING:
                            self.tokenList[i] = ( self.STRING, value )
                            varFound = True
                        breakOut = True
                    else:
                        afterEqual = True
                else:
                    afterEqual = False
            if not breakOut:
                i = i + 1
        index = i
        #  If the variable was not found, we need to add it.
        if not varFound:
            #  Back up to the previous non-whitespace item.  This is either another parameter
            #  or an open brace...doesn't matter which.
            index = index - 1
            while self.tokenList[index][0] == self.WHITESPACE:
                index = index - 1
            #  Finally, advance once again to find our insert point.
            index = index + 1
            #  The comment will follow the whitespace before the item.
            commentLoc = index + 1
            #  Insert the parameter.
            i = index
            self.tokenList.insert( i, ( self.WHITESPACE, gotSectionParamHint ) )
            i = i + 1
            self.tokenList.insert( i, ( self.STRING, name ) )
            i = i + 1
            self.tokenList.insert( i, ( self.WHITESPACE, gotEqualSignHint ) )
            i = i + 1
            self.tokenList.insert( i, ( self.EQUAL_SIGN, "=" ) )
            i = i + 1
            self.tokenList.insert( i, ( self.WHITESPACE, gotValueHint ) )
            i = i + 1
            self.tokenList.insert( i, ( self.STRING, value ) )
        #  If a comment is specified, add it followed by whitespace that is a single newline
        #  and whatever indentation follows the last newline of the pre-global whitespace.
        if not comment == None:
            #  First makes sure that an exact duplicate of this comment does not already
            #  exist.  This might happen if whatever software is doing this has already
            #  edited the same parameter.
            i = commentLoc - 1
            if i < 1 or not self.tokenList[i-1][0] == self.COMMENT or not self.tokenList[i-1][1] == gotCommentSequence + comment:
                i = commentLoc
                self.tokenList.insert( i, ( self.COMMENT, gotCommentSequence + comment ) )
                i = i + 1
                sub = gotSectionParamHint.rfind( "\n" )
                if sub == -1:
                    self.tokenList.insert( i, ( self.WHITESPACE, "\n" + gotSectionParamHint ) )
                else:
                    self.tokenList.insert( i, ( self.WHITESPACE, "\n" + gotSectionParamHint[sub+1:] ) )
        return
        
    #<!------------------------------------------------------------------------>
    ## Delete a global parameter.
    #
    #  @param name Name of the global parameter
    #
    #  Locate and delete a global parameter from the .v2d data.  Any comment
    #  that appears to be associated with the parameter is deleted as well.
    #  This function has no effect if the parameter does not exist.
    #<!------------------------------------------------------------------------>
    def deleteGlobal( self, name ):
        #  Get the index to this parameter
        loc = self.locateGlobal( name )
        if loc == None:
            #  Parameter doesn't exist.  Nothing to do.
            return
        i = loc[0]
        startIdx = i
        endIdx = i + 1
        #  All comments that IMMEDIATELY precede the parameter are assumed to be associated
        #  with it.  Any such comments match the indentation of the parameter and have single
        #  newlines between them.  First of all, we need whitespace as the preceding token.
        if i > 0 and self.tokenList[i-1][0] == self.WHITESPACE:
            #  Find the level of indent of this parameter.
            sub = self.tokenList[i-1][1].rfind( "\n" )
            if sub == -1:
                #  No newline in the whitespace - just get rid of it and ignore comments.
                startIdx = i - 1
            else:
                #  Got an indent.  Now back up over all comments that match this indent.
                #  Each comment line will have a whitespace after it.
                indent = self.tokenList[i-1][1][sub+1:]
                keepGoing = True
                while keepGoing and i > 2 and self.tokenList[i-1][0] == self.WHITESPACE and self.tokenList[i-2][0] == self.COMMENT and self.tokenList[i-3][0] == self.WHITESPACE:
                    sub = self.tokenList[i-3][1].rfind( "\n" )
                    if sub == -1:
                        keepGoing = False
                    elif self.tokenList[i-1][1] == "\n" + indent and self.tokenList[i-3][1][sub+1:] == indent:
                        i = i - 2
                    else:
                        keepGoing = False
                #  Finally, get rid of whitespace before the parameter (and comments).
                if i > 0 and self.tokenList[i-1][0] == self.WHITESPACE:
                    i = i - 1
                startIdx = i
        else:
            #  No comments, no whitespace, not sure what's going on.  The first deleted item
            #  will be the parameter itself.
            startIdx = i
        #  Next find the end of the parameter - the value.  
        while endIdx < len( self.tokenList ) and not self.tokenList[endIdx][0] == self.STRING:
            endIdx = endIdx + 1
        if endIdx < len( self.tokenList ):
            endIdx = endIdx + 1
        del self.tokenList[startIdx:endIdx]

    #<!------------------------------------------------------------------------>
    ## Delete a complete section.
    #
    #  @param type Section type as a string, or a tuple of type and name.
    #  @param name Section name (if previous argument isn't a tuple containing it)
    #
    #  Locate and delete a section of specified type and name.  Any comments
    #  that appears to be associated with the section are deleted as well.
    #  This function has no effect if the section does not exist.
    #<!------------------------------------------------------------------------>
    def deleteSection( self, sType, name = None ):
        #  Get the index to the section
        i = self.getSectionIndex( sType, name )
        if i == None:
            #  Section doesn't exist.  Nothing to do.
            return
        startIdx = i
        endIdx = i
        #  All comments that IMMEDIATELY precede the parameter are assumed to be associated
        #  with it.  Any such comments match the indentation of the parameter and have single
        #  newlines between them.  First of all, we need whitespace as the preceding token.
        if i > 0 and self.tokenList[i-1][0] == self.WHITESPACE:
            #  Find the level of indent of this parameter.
            sub = self.tokenList[i-1][1].rfind( "\n" )
            if sub == -1:
                #  No newline in the whitespace - just get rid of it and ignore comments.
                startIdx = i - 1
            else:
                #  Got an indent.  Now back up over all comments that match this indent.
                #  Each comment line will have a whitespace after it.
                indent = self.tokenList[i-1][1][sub+1:]
                keepGoing = True
                while keepGoing and i > 2 and self.tokenList[i-1][0] == self.WHITESPACE and self.tokenList[i-2][0] == self.COMMENT and self.tokenList[i-3][0] == self.WHITESPACE:
                    sub = self.tokenList[i-3][1].rfind( "\n" )
                    if sub == -1:
                        keepGoing = False
                    elif self.tokenList[i-1][1] == "\n" + indent and self.tokenList[i-3][1][sub+1:] == indent:
                        i = i - 2
                    else:
                        keepGoing = False
                #  Finally, get rid of whitespace before the parameter (and comments).
                if i > 0 and self.tokenList[i-1][0] == self.WHITESPACE:
                    i = i - 1
                startIdx = i
        else:
            #  No comments, no whitespace, not sure what's going on.  The first deleted item
            #  will be the parameter itself.
            startIdx = i
        #  Locate the first closing brace that follows the section.  This
        #  should be its end.
        while endIdx < len( self.tokenList ) and not self.tokenList[endIdx][0] == self.CLOSE_BRACE:
            endIdx = endIdx + 1
        if endIdx < len( self.tokenList ):
            endIdx = endIdx + 1
        del self.tokenList[startIdx:endIdx]

    #<!------------------------------------------------------------------------>
    ## Delete a named parameter within a section.
    #
    #  @param type Section type as a string, or a tuple of type and name.
    #  @param sName Section name (included only if previous argument isn't a tuple containing it)
    #  @param name Parameter name
    #
    #  Locate and delete a named parameter within a section of specified type and name.  Any comments
    #  that appears to be associated with the parameter are deleted as well.
    #  This function has no effect if the section or parameter does not exist.
    #<!------------------------------------------------------------------------>
    def deleteSectionParam( self, sType, sName, name = None ):
        #  Adjust for a tuple first argument.
        if name == None:
            name = sName
            sName = None
        #  Get the index to the section
        i = self.getSectionIndex( sType, sName )
        if i == None:
            #  Section doesn't exist.  Nothing to do.
            return
        #  Skip to the first open brace.
        while i < len( self.tokenList ) and not self.tokenList[i][0] == self.OPEN_BRACE:
            i = i + 1
        #  Now locate the parameter.
        afterEqual = False
        found = False
        while i < len( self.tokenList ) and not found and not self.tokenList[i][0] == self.CLOSE_BRACE:
            if self.tokenList[i][0] == self.EQUAL_SIGN:
                afterEqual = True
            elif self.tokenList[i][0] == self.STRING:
                if afterEqual:
                    afterEqual = False
                elif self.tokenList[i][1] == name:
                    found = True
            if not found:
                i = i + 1
        #  Parameter not found?
        if i == len( self.tokenList ) or self.tokenList[i][0] == self.CLOSE_BRACE:
            return
        #  Got the parameter!
        startIdx = i
        endIdx = i
        #  All comments that IMMEDIATELY precede the parameter are assumed to be associated
        #  with it.  Any such comments match the indentation of the parameter and have single
        #  newlines between them.  First of all, we need whitespace as the preceding token.
        if i > 0 and self.tokenList[i-1][0] == self.WHITESPACE:
            #  Find the level of indent of this parameter.
            sub = self.tokenList[i-1][1].rfind( "\n" )
            if sub == -1:
                #  No newline in the whitespace - just get rid of it and ignore comments.
                startIdx = i - 1
            else:
                #  Got an indent.  Now back up over all comments that match this indent.
                #  Each comment line will have a whitespace after it.
                indent = self.tokenList[i-1][1][sub+1:]
                keepGoing = True
                while keepGoing and i > 2 and self.tokenList[i-1][0] == self.WHITESPACE and self.tokenList[i-2][0] == self.COMMENT and self.tokenList[i-3][0] == self.WHITESPACE:
                    sub = self.tokenList[i-3][1].rfind( "\n" )
                    if sub == -1:
                        keepGoing = False
                    elif self.tokenList[i-1][1] == "\n" + indent and self.tokenList[i-3][1][sub+1:] == indent:
                        i = i - 2
                    else:
                        keepGoing = False
                #  Finally, get rid of whitespace before the parameter (and comments).
                if i > 0 and self.tokenList[i-1][0] == self.WHITESPACE:
                    i = i - 1
                startIdx = i
        else:
            #  No comments, no whitespace, not sure what's going on.  The first deleted item
            #  will be the parameter itself.
            startIdx = i
        #  Next find the end of the parameter - the value.
        endIdx = endIdx + 1  
        while endIdx < len( self.tokenList ) and not self.tokenList[endIdx][0] == self.STRING:
            endIdx = endIdx + 1
        if endIdx < len( self.tokenList ):
            endIdx = endIdx + 1
        del self.tokenList[startIdx:endIdx]

    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a global variable.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a global parameter name.  If the whitespace has not already been set,
    #  it will be determined using findGlobalHint().  The user can set it
    #  explicitly using setGlobalHint().
    #<!------------------------------------------------------------------------>
    def getGlobalHint( self ):
        if self.globalHint == None:
            self.globalHint = self.findGlobalHint()
        return self.globalHint
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a parameter value.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a parameter value, after the equal sign.  If the whitespace has not already been set,
    #  it will be determined using findValueHint().  The user can set it
    #  explicitly using setValueHint().
    #<!------------------------------------------------------------------------>
    def getValueHint( self ):
        if self.valueHint == None:
            self.valueHint = self.findValueHint()
        return self.valueHint
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede an equal sign.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  an equal sign in a parameter setting.  If the whitespace has not already been set,
    #  it will be determined using findEqualSignHint().  The user can set it
    #  explicitly using setEqualSignHint().
    #<!------------------------------------------------------------------------>
    def getEqualSignHint( self ):
        if self.equalSignHint == None:
            self.equalSignHint = self.findEqualSignHint()
        return self.equalSignHint
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a section definition.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a section type string - basically before a section.  If the whitespace
    #  has not already been set, it will be determined using findSectionTypeHint().  
    #  The user can set it explicitly using setSectionTypeHint().
    #<!------------------------------------------------------------------------>
    def getSectionTypeHint( self ):
        if self.sectionTypeHint == None:
            self.sectionTypeHint = self.findSectionTypeHint()
        return self.sectionTypeHint
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a section name.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a section name (after the section type).  If the whitespace has not already been set,
    #  it will be determined using findSectionNameHint().  The user can set it
    #  explicitly using setSectionNameHint().
    #<!------------------------------------------------------------------------>
    def getSectionNameHint( self ):
        if self.sectionNameHint == None:
            self.sectionNameHint = self.findSectionNameHint()
        return self.sectionNameHint
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede an open brace.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  an open brace in a section definition.  If the whitespace has not already been set,
    #  it will be determined using findOpenBraceHint().  The user can set it
    #  explicitly using setOpenBraceHint().
    #<!------------------------------------------------------------------------>
    def getOpenBraceHint( self ):
        if self.openBraceHint == None:
            self.openBraceHint = self.findOpenBraceHint()
        return self.openBraceHint
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a close brace.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a close brace in a section definition.  If the whitespace has not already been set,
    #  it will be determined using findCloseBraceHint().  The user can set it
    #  explicitly using setEqualSignHint().
    #<!------------------------------------------------------------------------>
    def getCloseBraceHint( self ):
        if self.closeBraceHint == None:
            self.closeBraceHint = self.findCloseBraceHint()
        return self.closeBraceHint
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a parameter within a section.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a parameter setting within a "generic" section.  Different whitespace settings can exist
    #  for specific sections - this setting is used for those that don't have
    #  any specifics.  If the whitespace has not already been set,
    #  it will be determined using findSectionParameterHint().  The user can set it
    #  explicitly using setSectionParameterHint().
    #<!------------------------------------------------------------------------>
    def getSectionParameterHint( self ):
        if self.sectionParameterHint == None:
            self.sectionParameterHint = self.findSectionParameterHint()
        return self.sectionParameterHint
        
    #<!------------------------------------------------------------------------>
    ## Return the string added to the beginning of user-inserted comments.
    #
    #  @return     The current string inserted before comments.
    #
    #  This function returns the string that will be inserted before user-specified
    #  comments (these are included in functions that edit or create parameters).
    #  This string assures that the comment is interpreted as a comment.
    #<!------------------------------------------------------------------------>
    def getCommentSequence( self ):
        if self.commentSequence == None:
            self.commentSequence = self.findCommentSequence()
        return self.commentSequence
        
    #<!------------------------------------------------------------------------>
    ## Set a string of whitespace that should precede a global variable.
    #
    #  @param setting Whitespace to be used prior to global variables.
    #
    #  This function sets a string of whitespace that should appear before
    #  a global parameter name.  Any previous such settings are lost.
    #<!------------------------------------------------------------------------>
    def setGlobalHint( self, setting ):
        self.globalHint = setting
        
    #<!------------------------------------------------------------------------>
    ## Set a string of whitespace that should precede a parameter value.
    #
    #  @param setting Whitespace to be used prior to a parameter value.
    #
    #  This function sets a string of whitespace that should appear before
    #  a parameter value, after the equal sign.  Any previous such settings are lost.
    #<!------------------------------------------------------------------------>
    def setValueHint( self, setting ):
        self.valueHint = setting
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede an equal sign.
    #
    #  @param setting Whitespace to be used prior to an equal sign.
    #
    #  This function sets a string of whitespace that should appear before
    #  an equal sign in a parameter setting.  Any previous such settings are lost.
    #<!------------------------------------------------------------------------>
    def setEqualSignHint( self, setting ):
        self.equalSignHint = setting
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a section definition.
    #
    #  @param setting Whitespace to be used prior to a section definition.
    #
    #  This function sets a string of whitespace that should appear before
    #  a section type string - basically before a section.  Any previous such settings are lost.
    #<!------------------------------------------------------------------------>
    def setSectionTypeHint( self, setting ):
        self.sectionTypeHint = setting
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a section name.
    #
    #  @param setting Whitespace to be used prior to a section name.
    #
    #  This function sets a string of whitespace that should appear before
    #  a section name (after the section type).  Any previous such settings are lost.
    #<!------------------------------------------------------------------------>
    def setSectionNameHint( self, setting ):
        self.sectionNameHint = setting
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede an open brace.
    #
    #  @param setting Whitespace to be used prior to an open brace.
    #
    #  This function sets a string of whitespace that should appear before
    #  an open brace in a section definition.  Any previous such settings are lost.
    #<!------------------------------------------------------------------------>
    def setOpenBraceHint( self, setting ):
        self.openBraceHint = setting
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a close brace.
    #
    #  @param setting Whitespace to be used prior to a close brace.
    #
    #  This function sets a string of whitespace that should appear before
    #  a close brace in a section definition.  Any previous such settings are lost.
    #<!------------------------------------------------------------------------>
    def setCloseBraceHint( self, setting ):
        self.closeBraceHint = setting
        
    #<!------------------------------------------------------------------------>
    ## Return a string of whitespace that should precede a parameter within a section.
    #
    #  @param setting Whitespace to be used prior to a parameter within a section.
    #
    #  This function sets a string of whitespace that should appear before
    #  a parameter setting within a "generic" section.  Different whitespace settings can exist
    #  for specific sections - this setting is used for those that don't have
    #  any specifics.  Any previous such settings are lost.
    #<!------------------------------------------------------------------------>
    def setSectionParameterHint( self, setting ):
        self.sectionParameterHint = setting
        
    #<!------------------------------------------------------------------------>
    ## Set the string that is added before user-specified comments.
    #
    #  @param setting String that is inserted before comments - should contain "#".
    #
    #  This function sets a string that will be inserted before user-specified
    #  comments (included in the parameter setting functions).  This string
    #  should include the "#" character at the very least, as <i>vex2difx</i>
    #  interprets anything following this character as a comment.  Anything
    #  else may cause <i>vex2difx</i> to fail.
    #<!------------------------------------------------------------------------>
    def setCommentSequence( self, setting ):
        self.commentSequence = setting
        
    #<!------------------------------------------------------------------------>
    ## Decide on a string of whitespace that should precede a global variable.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a global parameter name.  The whitespace is determined in this function,
    #  based on what already appears in existing .v2d data by picking what is
    #  already done, or what is mostly done.  If there is nothing available in
    #  the data to base this decision on, a default string containing a single
    #  newline character is used.
    #
    #  This function should not be used to add whitespace to .v2d data - that
    #  should be done using getGlobalHint(), which calls this function only
    #  when no global hint has already been set.
    #<!------------------------------------------------------------------------>
    def findGlobalHint( self ):
        #  Make a list of all of the different forms of whitespace the precede
        #  a global parameter, and the number of times they appear.  There may
        #  well be none.
        braceCount = 0
        valueExpected = False
        testList = {}
        for idx in range( len( self.tokenList ) ):
            item = self.tokenList[idx]
            if item[0] == self.OPEN_BRACE:
                braceCount = braceCount + 1
            elif item[0] == self.CLOSE_BRACE:
                braceCount = braceCount - 1
            elif item[0] == self.EQUAL_SIGN:
                valueExpected = True
            elif item[0] == self.STRING:
                if valueExpected:
                    valueExpected = False
                elif braceCount == 0:
                    #  This may be a global parameter name - see if it is followed by an equal
                    #  sign.
                    testI = idx + 1
                    while testI < len( self.tokenList ) and ( self.tokenList[testI][0] == self.WHITESPACE or self.tokenList[testI][0] == self.COMMENT ):
                        testI = testI + 1
                    if testI < len( self.tokenList ) and self.tokenList[testI][0] == self.EQUAL_SIGN:
                        #  Definately a global parameter name. Look at the whitespace ahead
                        #  of it.
                        i = idx - 1
                        if not i == -1:
                            try:
                                testList[self.tokenList[i][1]] = testList[self.tokenList[i][1]] + 1
                            except KeyError:
                                testList[self.tokenList[i][1]] = 1
        #  This is the default value
        ret = "\n"
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Decide on a string of whitespace that should precede a parameter value.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a parameter value (i.e. after an equal sign).  The whitespace is determined in this function,
    #  based on what already appears in existing .v2d data by picking what is
    #  already done, or what is mostly done.  If there is nothing available in
    #  the data to base this decision on, a default string containing a single
    #  space character is used.
    #
    #  This function should not be used to add whitespace to .v2d data - that
    #  should be done using getValueHint(), which calls this function only
    #  when no value hint has already been set.
    #<!------------------------------------------------------------------------>
    def findValueHint( self ):
        #  Search for equal signs, then locate the first string that follows them.
        #  The whitespace just before that is added to a list of whitespace types.
        testList = {}
        valueExpected = False
        for idx in range( len( self.tokenList ) ):
            item = self.tokenList[idx]
            if item[0] == self.EQUAL_SIGN:
                valueExpected = True
            elif item[0] == self.STRING and valueExpected:
                valueExpected = False
                i = idx - 1
                if not i == 0 and self.tokenList[i][0] == self.WHITESPACE:
                    try:
                        testList[self.tokenList[i][1]] = testList[self.tokenList[i][1]] + 1
                    except KeyError:
                        testList[self.tokenList[i][1]] = 1
        #  This is the default value
        ret = " "
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Decide on a string of whitespace that should precede an equal sign.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  an equal sign in a parameter setting (i.e. after a parameter name), either
    #  as a global or inside a section.  The whitespace is determined in this function,
    #  based on what already appears in existing .v2d data by picking what is
    #  already done, or what is mostly done.  If there is nothing available in
    #  the data to base this decision on, a default string containing a single
    #  space character is used.
    #
    #  This function should not be used to add whitespace to .v2d data - that
    #  should be done using getEqualSignHint(), which calls this function only
    #  when no equal sign hint has already been set.
    #<!------------------------------------------------------------------------>
    def findEqualSignHint( self ):
        #  Search for equal signs  The whitespace just before is added to a list 
        #  of whitespace types.
        testList = {}
        valueExpected = False
        for idx in range( len( self.tokenList ) ):
            item = self.tokenList[idx]
            if item[0] == self.EQUAL_SIGN:
                valueExpected = True
            elif item[0] == self.STRING and valueExpected:
                valueExpected = False
                i = idx - 1
                if not i == 0 and self.tokenList[i][0] == self.WHITESPACE:
                    try:
                        testList[self.tokenList[i][1]] = testList[self.tokenList[i][1]] + 1
                    except KeyError:
                        testList[self.tokenList[i][1]] = 1
        #  This is the default value
        ret = " "
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
            
    #<!------------------------------------------------------------------------>
    ## Decide on a string of whitespace that should precede a section type.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  the type of a new section - this will be the separation between sections
    #  as the type is the first thing encountered.  The whitespace is determined
    #  based on what already appears in existing .v2d data by picking what is
    #  already done, or what is mostly done.  If there is nothing available in
    #  the data to base this decision on, a default string containing two
    #  newline characters is used.
    #
    #  This function should not be used to add whitespace to .v2d data - that
    #  should be done using getSectionTypeHint(), which calls this function only
    #  when no section type hint has already been set.
    #<!------------------------------------------------------------------------>
    def findSectionTypeHint( self ):
        #  Locate section types in the token strings.  It appears first.  We
        #  count braces to make sure we are not within braces when the type and
        #  name are located (just in case the .v2d data contain something insidious).
        #  Make a list of the different types of whitespace that precede type
        #  specifications and count them.
        bracesLevel = 0
        valueExpected = False
        testList = {}
        for i in range( len( self.tokenList ) ):
            item = self.tokenList[i]
            if item[0] == self.OPEN_BRACE:
                bracesLevel = bracesLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif item[0] == self.EQUAL_SIGN:
                valueExpected = True
            elif item[0] == self.STRING:
                #  This might be a value...
                if valueExpected:
                    valueExpected = False
                elif bracesLevel == 0:
                    #  If the next non-whitespace, non-comment item is a string, this is a type.
                    look = i + 1
                    while look < len( self.tokenList ) and ( self.tokenList[look][0] == self.WHITESPACE or self.tokenList[look][0] == self.COMMENT ):
                        look = look + 1
                    if i > 0 and look < len( self.tokenList ) and self.tokenList[look][0] == self.STRING:
                        try:
                            testList[self.tokenList[i - 1][1]] = testList[self.tokenList[i - 1][1]] + 1
                        except KeyError:
                            testList[self.tokenList[i - 1][1]] = 1
        #  This is the default value
        ret = "\n\n"
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Decide on a string of whitespace that should precede a section name.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  the name of a new section, i.e. between the section type specification and
    #  the name.  The whitespace is determined
    #  based on what already appears in existing .v2d data by picking what is
    #  already done, or what is mostly done.  If there is nothing available in
    #  the data to base this decision on, a default string containing a single
    #  space character is used.
    #
    #  This function should not be used to add whitespace to .v2d data - that
    #  should be done using getSectionNameHint(), which calls this function only
    #  when no section name hint has already been set.
    #<!------------------------------------------------------------------------>
    def findSectionNameHint( self ):
        #  Locate section types in the token strings.  It appears first.  We
        #  count braces to make sure we are not within braces when the type and
        #  name are located (just in case the .v2d data contain something insidious).
        #  Make a list of the different types of whitespace that precede type
        #  specifications and count them.
        bracesLevel = 0
        valueExpected = False
        testList = {}
        for i in range( len( self.tokenList ) ):
            item = self.tokenList[i]
            if item[0] == self.OPEN_BRACE:
                bracesLevel = bracesLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                bracesLevel = bracesLevel - 1
            elif item[0] == self.EQUAL_SIGN:
                valueExpected = True
            elif item[0] == self.STRING:
                #  This might be a value...
                if valueExpected:
                    valueExpected = False
                elif bracesLevel == 0:
                    #  If the next non-whitespace, non-comment item is a string, this is a
                    #  section specification.
                    look = i + 1
                    while look < len( self.tokenList ) and ( self.tokenList[look][0] == self.WHITESPACE or self.tokenList[look][0] == self.COMMENT ):
                        look = look + 1
                    if look < len( self.tokenList ) and self.tokenList[look][0] == self.STRING:
                        try:
                            testList[self.tokenList[look - 1][1]] = testList[self.tokenList[look - 1][1]] + 1
                        except KeyError:
                            testList[self.tokenList[look - 1][1]] = 1
        #  This is the default value
        ret = " "
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Decide on a string of whitespace that should precede an open brace.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  an open brace.  The whitespace is determined in this function,
    #  based on what already appears in existing .v2d data by picking what is
    #  already done, or what is mostly done.  If there is nothing available in
    #  the data to base this decision on, a default string containing a single
    #  space character is used.
    #
    #  This function should not be used to add whitespace to .v2d data - that
    #  should be done using getOpenBraceHint(), which calls this function only
    #  when no open brace hint has already been set.
    #<!------------------------------------------------------------------------>
    def findOpenBraceHint( self ):
        #  Search for any open braces  The whitespace just before is added to a list 
        #  of whitespace types.
        testList = {}
        for idx in range( len( self.tokenList ) ):
            item = self.tokenList[idx]
            if item[0] == self.OPEN_BRACE:
                i = idx - 1
                if not i == 0 and self.tokenList[i][0] == self.WHITESPACE:
                    try:
                        testList[self.tokenList[i][1]] = testList[self.tokenList[i][1]] + 1
                    except KeyError:
                        testList[self.tokenList[i][1]] = 1
        #  This is the default value
        ret = " "
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Decide on a string of whitespace that should precede a close brace.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a close brace.  The whitespace is determined in this function,
    #  based on what already appears in existing .v2d data by picking what is
    #  already done, or what is mostly done.  If there is nothing available in
    #  the data to base this decision on, a default string containing a newline
    #  character is used.
    #
    #  This function should not be used to add whitespace to .v2d data - that
    #  should be done using getCloseBraceHint(), which calls this function only
    #  when no close brace hint has already been set.
    #<!------------------------------------------------------------------------>
    def findCloseBraceHint( self ):
        #  Search for any close braces  The whitespace just before is added to a list 
        #  of whitespace types.
        testList = {}
        for idx in range( len( self.tokenList ) ):
            item = self.tokenList[idx]
            if item[0] == self.CLOSE_BRACE:
                i = idx - 1
                if not i == 0 and self.tokenList[i][0] == self.WHITESPACE:
                    try:
                        testList[self.tokenList[i][1]] = testList[self.tokenList[i][1]] + 1
                    except KeyError:
                        testList[self.tokenList[i][1]] = 1
        #  This is the default value
        ret = "\n"
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Decide on a string of whitespace that should precede a parameter within a section.
    #
    #  @return     A string of whitespace.  This will never be None.
    #
    #  This function returns a string of whitespace that should appear before
    #  a the name of a parameter within a section.  The whitespace is determined in this function,
    #  based on what already appears in existing .v2d data by picking what is
    #  already done, or what is mostly done.  If there is nothing available in
    #  the data to base this decision on, a default string containing a newline
    #  followed by four spaces is used.
    #
    #  This function should not be used to add whitespace to .v2d data - that
    #  should be done using getSectionParameterHint(), which calls this function only
    #  when no such hint has already been set.
    #<!------------------------------------------------------------------------>
    def findSectionParameterHint( self ):
        #  Search for parameter names within sections.  These occur within braces
        #  and before equal signs.  The whitespace just before is added to a list 
        #  of whitespace types.
        testList = {}
        afterEqual = False
        braceLevel = 0
        for idx in range( len( self.tokenList ) ):
            item = self.tokenList[idx]
            if item[0] == self.OPEN_BRACE:
                braceLevel = braceLevel + 1
            elif item[0] == self.CLOSE_BRACE:
                braceLevel = braceLevel - 1
            elif item[0] == self.EQUAL_SIGN:
                afterEqual = True
            elif item[0] == self.STRING:
                #  The string should be before an equal sign and within braces
                #  to be a section parameter.
                if afterEqual:
                    afterEqual = False
                elif braceLevel > 0:
                    i = idx - 1
                    if not i == 0 and self.tokenList[i][0] == self.WHITESPACE:
                        try:
                            testList[self.tokenList[i][1]] = testList[self.tokenList[i][1]] + 1
                        except KeyError:
                            testList[self.tokenList[i][1]] = 1
        #  This is the default value
        ret = "\n    "
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Decide on a string to be added prior to user-specified comments.
    #
    #  @return     The string that is added prior to user-specified comments.
    #
    #  This function returns a string that is added prior to user-specified
    #  comments (as can be done in the parameter setting functions like setGloba())
    #  that indicate the rest of the line should be a comment.  What this
    #  string should be is determined by first looking at all of the comments
    #  in the existing .v2d data and seeing what is most common.  If there is
    #  no such data, or no comments in it to use as a model, a "sensible"
    #  default is used.
    #<!------------------------------------------------------------------------>
    def findCommentSequence( self ):
        #  Make a list of all of the comment sequences (the characters from
        #  the comment character "#" until something other than spaces or tabs)
        #  in the current .v2d data and how many times each appears.
        testList = {}
        for item in self.tokenList:
            if item[0] == self.COMMENT:
                i = 0
                testStr = ""
                while i < len( item[1] ) and ( item[1][i] == "#" or item[1][i] == " " or item[1][i] == '\t' ):
                    testStr = testStr + item[1][i]
                    i = i + 1
                try:
                    testList[testStr] = testList[testStr] + 1
                except KeyError:
                    testList[testStr] = 1
        #  This is the default value
        ret = "#  "
        num = 0
        for string in testList.keys():
            if testList[string] > num:
                num = testList[string]
                ret = string
        return ret
        
    #<!------------------------------------------------------------------------>
    ## Add a comment at a given location in the data.
    #
    #  @param comment The text of the comment.
    #  @param location Optional string location in the file, one of "TOP", "HEAD", or "FOOT".
    #
    #  Add the given comment to the .v2d file at the specified location.  The comment
    #  is a string that will have the "comment sequence" inserted before it and
    #  a newline added after it.  The location is optional - it is one of the
    #  following strings:
    #  <ul>
    #  <li>"TOP" will put the comment at the very beginning of the .v2d data.
    #  <li>"HEAD" will add the comment to the end of any "header" comments.
    #  <li>"FOOT" will add the comment to the end of the .v2d data.
    #  </ul>
    #  By default the location is "HEAD".
    #
    #  Existing comments are not duplicated - if a comment already exists that exactly
    #  matches what is being added, this function will not add the new comment.
    #<!------------------------------------------------------------------------>
    def setComment( self, comment, location = None ):
        if location == None:
            location = "HEAD"
        print ">>>>> Adding \"" + comment + "\""
        #  Not sure why this would happen exactly.
        if comment == None:
            return
        insertStr = self.getCommentSequence() + comment
        backwardsInsert = False
        existingString = ""
        #  Find an insert point for this comment, based on the specified location.
        if location == "TOP":
            insertPt = 0
            if len( self.tokenList ) > 0:
                existingString = self.tokenList[0][1]
        elif location == "FOOT":
            insertPt = len( self.tokenList )
            if len( self.tokenList ) > 1:
                existingString = self.tokenList[len( self.tokenList ) - 2][1]
        elif location == "HEAD":
            #  This is the only tricky location.  Find the end of any existing
            #  header.
            i = 0
            bailOut = False
            while i < len( self.tokenList ) and not bailOut:
                if self.tokenList[i][0] == self.COMMENT:
                    if i + 1 == len( self.tokenList ) or ( self.tokenList[i+1][0] == self.WHITESPACE and self.tokenList[i+1][1] == "\n" ):
                        existingString = self.tokenList[i][1]
                        i = i + 2
                    else:
                        existingString = self.tokenList[i][1]
                        backwardsInsert = True
                        i = i + 1
                        bailOut = True
                else:
                    bailOut = True
            insertPt = i
        #  Make sure this comment isn't already there.
        if existingString == insertStr:
            return
        #  Add the comment.
        if backwardsInsert:
            self.tokenList.insert( insertPt, ( self.WHITESPACE, "\n" ) )
            self.tokenList.insert( insertPt + 1, ( self.COMMENT, insertStr ) )
        else:
            self.tokenList.insert( insertPt, ( self.COMMENT, insertStr ) )
            self.tokenList.insert( insertPt + 1, ( self.WHITESPACE, "\n" ) )
        return
            
    #<!------------------------------------------------------------------------>
    ## Locate all comments containing a given string.
    #
    #  @param commentStr A string that will be searched for in comments.
    #  @param location Optional string location in the file, one of "TOP", "HEAD", or "FOOT".
    #  @return List of strings representing all matching comments.  An empty list is returned if none exist.
    #
    #  Locate and return as a list of strings all comments in the .v2d data that contain the given string.  An
    #  optional location string can restrict the search to different sections
    #  of the file.  If the search string is None or empty, ALL comments that match the location will
    #  be returned.
    #<!------------------------------------------------------------------------>
    def getComments( self, commentStr = None, location = None ):
        ret = []
        tempList = []
        if location == None:
            location = "ALL"
        #  Make a list of comments that match the location directive.  If we are only
        #  looking at the TOP, this is a single line.
        if not location.upper() == "ALL" and location.upper() == "TOP":
            if len( self.tokenList ) > 0 and self.tokenList[0][0] == self.COMMENT:
                tempList.append( self.tokenList[0][1] )
        #  Comments in the header are all that are piled at the top of the data with
        #  newline separators.
        if location.upper() == "ALL" or location.upper() == "HEAD":
            i = 0
            bailOut = False
            while i < len( self.tokenList ) and not bailOut:
                if self.tokenList[i][0] == self.COMMENT:
                    if i + 1 == len( self.tokenList ) or ( self.tokenList[i+1][0] == self.WHITESPACE and self.tokenList[i+1][1] == "\n" ):
                        tempList.append( self.tokenList[i][1] )
                        i = i + 2
                    else:
                        tempList.append( self.tokenList[i][1] )
                        bailOut = True
                else:
                    bailOut = True
        #  Footer comments are all piled at the bottom, with newline separators.
        if location.upper() == "ALL" or location.upper() == "FOOT":
            i = len( self.tokenList ) - 1
            bailOut = False
            keepLimit = len( self.tokenList )
            while i > -1 and not bailOut:
                if self.tokenList[i][0] == self.WHITESPACE and self.tokenList[i][1] == "\n":
                    if self.tokenList[i-1][0] == self.COMMENT:
                        keepLimit = i - 1
                        i = i - 2
                    else:
                        bailOut = True
                else:
                    bailOut = True
            while keepLimit < len( self.tokenList ):
                if self.tokenList[keepLimit][0] == self.COMMENT:
                    tempList.append( self.tokenList[keepLimit][1] )
                keepLimit = keepLimit + 1
        #  Now plow through this temporary list we made and keep all items that match the
        #  search string.
        if commentStr == None:
            ret = tempList
        else:
            i = 0
            while i < len( tempList ):
                if not tempList[i].find( commentStr ) == -1:
                    ret.append( tempList[i] )
                i = i + 1
        return ret
            
    #<!------------------------------------------------------------------------>
    ## Remove all comments containing a given string.
    #
    #  @param commentStr A string that will be searched for in comments.
    #  @param location Optional string location in the file, one of "TOP", "HEAD", or "FOOT".
    #
    #  Locate and delete all comments in the .v2d data that contain the given string.  An
    #  optional location string can restrict the search to different sections
    #  of the file.  If the search string is None or empty, ALL comments that
    #  match the location will be deleted.
    #<!------------------------------------------------------------------------>
    def deleteComments( self, commentStr, location = None ):
        tempList = []
        if location == None:
            location = "ALL"
        #  Make a list of the indexes of comments that match the location directive.  If we are only
        #  looking at the TOP, this is a single line.
        if not location.upper() == "ALL" and location.upper() == "TOP":
            if len( self.tokenList ) > 0 and self.tokenList[0][0] == self.COMMENT:
                tempList.append( 0 )
        #  Comments in the header are all that are piled at the top of the data with
        #  newline separators.
        if location.upper() == "ALL" or location.upper() == "HEAD":
            i = 0
            bailOut = False
            while i < len( self.tokenList ) and not bailOut:
                if self.tokenList[i][0] == self.COMMENT:
                    if i + 1 == len( self.tokenList ) or ( self.tokenList[i+1][0] == self.WHITESPACE and self.tokenList[i+1][1] == "\n" ):
                        tempList.append( i )
                        i = i + 2
                    else:
                        tempList.append( i )
                        bailOut = True
                else:
                    bailOut = True
        #  Footer comments are all piled at the bottom, with newline separators.
        if location.upper() == "ALL" or location.upper() == "FOOT":
            i = len( self.tokenList ) - 1
            bailOut = False
            keepLimit = len( self.tokenList )
            while i > -1 and not bailOut:
                if self.tokenList[i][0] == self.WHITESPACE and self.tokenList[i][1] == "\n":
                    if self.tokenList[i-1][0] == self.COMMENT:
                        keepLimit = i - 1
                        i = i - 2
                    else:
                        bailOut = True
                else:
                    bailOut = True
            while keepLimit < len( self.tokenList ):
                if self.tokenList[keepLimit][0] == self.COMMENT:
                    tempList.append( keepLimit )
                keepLimit = keepLimit + 1
        #  Make a new list of token indexes that have the correct matching comment strings.
        deleteList = []
        if commentStr == None:
            deleteList = tempList
        else:
            i = 0
            while i < len( tempList ):
                if not self.tokenList[tempList[i]][1].find( commentStr ) == -1:
                    deleteList.append( tempList[i] )
                i = i + 1
        #  Now plow through delete list we made and remove the items in the token list
        #  that match the indexes.  This is tricky because each delete changes the size of the
        #  token list, so the indexes have to be adjusted.  Also each comment is followed by
        #  whitespace so we have to delete two items.
        i = 0
        adj = 0
        while i < len( deleteList ):
            kill = deleteList[i] - adj
            del self.tokenList[kill:kill + 2]
            adj = adj + 2
            i = i + 1
        return
            
            
            
            
            
        
        
        
        
        
        

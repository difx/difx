AC_DEFUN([AC_PYTHON_MODULE_CHECK],[
	AC_MSG_CHECKING(for python module $1)
	define(`CMD_OUTPUT',
		esyscmd(python -c "import $1" 2>/dev/null) )
#	ret_val=sysval
#	test not vetted for all architectures
	ret_val=1
#	(echo "retval = $ret_val")
	AM_CONDITIONAL([HAVE_PYTHON_MODULE_$1], [test $ret_val -eq 0])
	if test $ret_val -eq 0;
	then
		AC_MSG_RESULT(yes)
	else
		AC_MSG_RESULT(no)
	fi
])

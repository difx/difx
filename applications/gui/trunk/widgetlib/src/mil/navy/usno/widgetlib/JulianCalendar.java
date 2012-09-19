/*
 * This is really a GregorianCalendar that can be set using Julian Dates.  It
 * also produces the Julian Date representation of its setting.  Modified Julian
 * Date (Julian Date - 2400000.5) is also provided and accepted as a setting.
 * 
 * This code is an adaptation of the corresponding C functions in the USNO NOVAS
 * package:
 *          Bangert, J., Puatua, W., Kaplan, G., Bartlett, J., Harris, W., 
 *          Fredericks, A., & Monet, A. (2011)
 *          User’s Guide to NOVAS Version C3.1 
 *          (Washington, DC: USNO).
 */
package mil.navy.usno.widgetlib;

/**
 *
 * @author jspitzak
 */
import java.util.Calendar;
import java.util.GregorianCalendar;

public class JulianCalendar extends GregorianCalendar {

    public static double MILLISEC_IN_DAY = 24.0 * 3600.0 * 1000.0;

    /*
     * Returns the Julian Date equivalent of the calendar setting.
     */
    public double julian() {
        int year = this.get( Calendar.YEAR );
        int month = this.get( Calendar.MONTH ) + 1;
        int day = this.get( Calendar.DAY_OF_MONTH );
        Calendar foo = new GregorianCalendar();
        foo.clear();
        foo.set( Calendar.YEAR, this.get( Calendar.YEAR ) );
        foo.set( Calendar.MONTH, this.get( Calendar.MONTH ) );
        foo.set( Calendar.DAY_OF_MONTH, this.get( Calendar.DAY_OF_MONTH ) );
        double dayFraction = ( this.getTimeInMillis() - foo.getTimeInMillis() ) / MILLISEC_IN_DAY;

        long jd12h = (long)day - 32075L + 1461L * ( (long)year + 4800L
                    + ( (long)month - 14L ) / 12L ) / 4L
                    + 367L * ( (long)month - 2L - ( (long)month - 14L ) / 12L * 12L )
                    / 12L - 3L * ( ( (long)year + 4900L + ( (long)month - 14L) / 12L )
                    / 100L ) / 4L;

        return ( (double)jd12h - 0.5 + dayFraction );
    }

    /*
     * Sets the calendar using a Julian Date.
     */
    public void julian( double inJulian ) {
        long jd, k, m, n;

        double djd;

        djd = inJulian + 0.5;
        jd = (long) djd;

        double dayFraction = ( djd - (double)jd ) * 24.0;

        k     = jd + 68569L;
        n     = 4L * k / 146097L;

        k     = k - ( 146097L * n + 3L ) / 4L;
        m     = 4000L * ( k + 1L ) / 1461001L;
        k     = k - 1461L * m / 4L + 31L;

        int month = (int)( 80L * k / 2447L );
        int day   = (int)( k - 2447L * (long)month / 80L );
        k      = (long)month / 11L;

        month = (int)( (long)month + 2L - 12L * k );
        int year  = (int)( 100L * ( n - 49L ) + m + k );

        this.clear();
        this.setTimeInMillis( (long)( MILLISEC_IN_DAY * ( dayFraction ) ) );
        this.set( Calendar.YEAR, year );
        this.set( Calendar.MONTH, month - 1 );
        this.set( Calendar.DAY_OF_MONTH, day );
    }
    
    public double mjd() {
        return julian() - 2400000.5;
    }
    
    public void mjd( double newDate ) {
        julian( newDate + 2400000.5 );
    }

}

#-------------------------------------------------------------------------------
# Download ERA5-Land total precipitation
# Written by: Jeff Wen
# Last edited by: Jessica Li
# 
# Run this in the CDS Toolbox to download daily values of total precipitation 
# from ERA5-Land. De-comment 5 years at a time and submit 3 requests. Then 
# stack and save the 3 files to create era5_2006-2020_daily_precip.nc.
#-------------------------------------------------------------------------------
import cdstoolbox as ct

@ct.application(title='Download data')
@ct.output.download()
def download_application():
    data = ct.catalogue.retrieve(
        'reanalysis-era5-land',
        {
            'variable': [
                'total_precipitation'
            ],
            'year': [
                '2006', '2007', '2008','2009', '2010',
                #'2011','2012','2013','2014','2015',
                #'2016','2017','2018','2019','2020'
            ],
            'month': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
            ],
            'day': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
                '13', '14', '15',
                '16', '17', '18',
                '19', '20', '21',
                '22', '23', '24',
                '25', '26', '27',
                '28', '29', '30',
                '31',
            ],
            'time': [
                '00:00', '01:00', '02:00',
                '03:00', '04:00', '05:00',
                '06:00', '07:00', '08:00',
                '09:00', '10:00', '11:00',
                '12:00', '13:00', '14:00',
                '15:00', '16:00', '17:00',
                '18:00', '19:00', '20:00',
                '21:00', '22:00', '23:00',
            ],
            'area': [
                49.92, -125.5, 24.43,-66.62,
            ],
        }
    )
    
    data_daily = ct.cube.resample(data, freq='D', how='sum', closed='right')

    return data_daily
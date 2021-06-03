# Ionospheric TEC anomaly as an earthquake precursor
This project is to discover the relationship between Ionospheric Total Free-Electron Content（TEC) anomaly and earthquakes in New Zealand.

The amount of TEC values varies with the amount of radiation received from the Sun.Thus, TEC values have seasonal variations, TEC values drop since there is less radiation received in winter, and increases in summer.It also has a strong daily fluctuation, TEC values are high during daytime and near-zero at night-time.Free-electrons imposes a dispersive delay on the GPS satellite ranging signals broadcasting.The affection on the ranging signals also varies with TEC values.

In addition, during earthquake preparations, free-electrons are released from the ground and change the local ionospheric TEC values in a relatively short period of time.When is the TEC anomaly occurs due to earthquake preparations remain controversial? Between 3 days and 2 weeks before and after the earthquakes are discussed in different research articles.

This project will start from collect the original GPS raw data.There are two types of data, one is the GPS observation data from local GeoNet, powered by GNS Science, the other is the navigation data, which is the ephemeris data for GPS constellation, from Scripps Orbit and Permanent Array Center in California. 
Both types of raw data are in receiver independent exchange(RINEX) format, which enables users to post-process the GPS stations received data and generates more accurate results.

GPS Toolkit(GPSTk), an open-source computing suite to the satellite navigation community produced by Space and Geophysics Laboratory within the Applied Research Laboratories of the University of Texas at Austin, to process GPS associated satellite navigation system data (https://gitlab.com/sgl-ut/GPSTk), is utilised to translate the raw GPS data.

There are three main programs from the GPSTk are used in this project.
1. ResCor, computes any residuals and correlations, and then register extended RINEX observation data.
2. Ionobias, continue reads the extended RINEX observation data to estimate satellite and receiver biases and to compute a simple ionospheric model using least squares and the slant TEC values.
3. TECMaps, reads the output data from both ResCor and IonoBias programs, to create maps of the TEC.

The assumption of this series of process is, the ionosphere is a thin shell about 350 kilometres height above the ground, by knowing this fixed vertical TEC height, the oblique angle of the GPS ranging signal, and the Ionospheric Pierce Point(IPP), where the ranging signal goes through the thin shell, the slant length from the TEC to the receiver can be computed. Put both of the computed factors and estimated factors that without TEC affections, into a least-square model, the TEC value at the corresponding IPP can be estimated. Finally, a TEC map is plotted throughout all gridded IPPs.

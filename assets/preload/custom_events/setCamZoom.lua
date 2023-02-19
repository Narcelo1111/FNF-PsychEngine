function onEvent(tag, val1, val2)
	if tag == 'setCamZoom' then 
        setProperty('defaultCamZoom', val1)
    end
end
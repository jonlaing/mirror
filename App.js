import React from 'react';
import { StyleSheet, Text, View } from 'react-native';
import AppRe from './lib/js/src/App';
import config from './config';


export default class App extends React.Component {
  render() {
    return (
      <View style={styles.container}>
        <AppRe weatherApiKey={config.weather.apiKey} />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#fff',
    alignItems: 'center',
    justifyContent: 'center',
  },
});

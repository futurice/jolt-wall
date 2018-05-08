import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Main.embed(document.getElementById('root'), {
  userToken: process.env.ELM_APP_USER_TOKEN,
  apiRoot: process.env.ELM_APP_API_ROOT,
  flowUrl: process.env.ELM_APP_FLOW_URL
});


registerServiceWorker();

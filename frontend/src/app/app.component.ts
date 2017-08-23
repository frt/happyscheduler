import { Component } from '@angular/core';
import { InfoMessagesService } from './info-messages.service';
import { PushNotificationsService } from 'angular2-notifications';

@Component({
    selector: 'app-root',
    templateUrl: './app.component.html',
    styleUrls: ['./app.component.css']
})
export class AppComponent {

    constructor(public messagesService: InfoMessagesService
                , private _pushNotifications: PushNotificationsService) {
        _pushNotifications.requestPermission();
    }
}
